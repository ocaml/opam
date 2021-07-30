#!/bin/bash -xue

. .github/scripts/preamble.sh

if [ "$GITHUB_EVENT_NAME" = "pull_request" ] && [ "x" = "x$BASE_REF_SHA$PR_REF_SHA" ] ; then
  echo "Variables BASE_REF_SHA and PR_REF_SHA must be defined in a pull request job"
  exit 2
fi
# Don't use BASE_REF_SHA and PR_REF_SHA on non pull request jobs, they are not
# defined. See .github/workflows/ci.yml hygiene job.

if [ "$GITHUB_EVENT_NAME" = "pull_request" ]; then
  # needed for git diffs and rev-list
  # we need to get history from base ref to head ref for check configure
  depth=10
  set +e
  git cat-file -e $BASE_REF_SHA
  r=$?
  while [ $r -ne 0 ] ; do
    git fetch origin $GITHUB_REF --depth=$depth
    depth=$(( $depth + 10 ))
    git cat-file -e $BASE_REF_SHA
    r=$?
  done
  set -e
fi

CheckConfigure () {
  GIT_INDEX_FILE=tmp-index git read-tree --reset -i "$1"
  git diff-tree --diff-filter=d --no-commit-id --name-only -r "$1" \
    | (while IFS= read -r path
  do
    case "$path" in
      configure|configure.ac|m4/*)
        touch CHECK_CONFIGURE;;
    esac
  done)
  rm -f tmp-index
  if [[ -e CHECK_CONFIGURE ]] ; then
    echo "configure generation altered in $1"
    echo 'Verifying that configure.ac generates configure'
    git clean -dfx
    git checkout -f "$1"
    mv configure configure.ref
    make configure
    if ! diff -q configure configure.ref >/dev/null ; then
      echo -e "[\e[31mERROR\e[0m] configure.ac in $1 doesn't generate configure, \
please run make configure and fixup the commit"
      ERROR=1
    else
      echo "configure ok for $1"
    fi
  fi
}

set +x

ERROR=0

###
# Check configure
###

(set +x ; echo -en "::group::check configure\r") 2>/dev/null
case $GITHUB_EVENT_NAME in
  push)
    CheckConfigure "$GITHUB_SHA"
    ;;
  pull_request)
    for commit in $(git rev-list $BASE_REF_SHA...$PR_REF_SHA --reverse)
    do
      echo "check configure for $commit"
      CheckConfigure "$commit"
    done
    ;;
  *)
    echo "no configure to check for unknown event"
    ;;
esac
(set +x ; echo -en "::endgroup::check configure\r") 2>/dev/null


###
# Check install.sh
###

if [[ $GITHUB_EVENT_NAME = 'pull_request' ]]; then
  if ! git diff "$BASE_REF_SHA..$PR_REF_SHA" --name-only --exit-code -- shell/install.sh > /dev/null ; then
    echo '::group::shell/install.sh updated - checking it'
    eval $(grep '^\(OPAM_BIN_URL_BASE\|DEV_VERSION\|VERSION\)=' shell/install.sh)
    echo "OPAM_BIN_URL_BASE = $OPAM_BIN_URL_BASE"
    echo "VERSION = $VERSION; DEV_VERSION = $DEV_VERSION"
    if [[ $DEV_VERSION != $VERSION ]]; then
      ARCHES=2
    else
      ARCHES=1
    fi
    current_tag=''

    while read -r line tag platform sha ; do
      if [[ $tag != $current_tag ]]; then
        current_tag="$tag"
        if [[ $tag = $VERSION || $tag = $DEV_VERSION ]]; then
          ((ARCHES--))
        fi
        echo "🐪 Checking binaries for opam ${tag//-/\~}"
      fi
      URL="$OPAM_BIN_URL_BASE$tag/opam-$tag-$platform"
      echo "Downloading $URL"
      check=$(curl -Ls "$URL" | sha512sum | cut -d' ' -f1)
      if [[ $check = $sha ]] ; then
        echo "         as expected ($sha)"
      else
        echo "::error file=shell/install.sh,line=$line::Incorrect checksum; got $check"
        ERROR=1
      fi
    done < <(gawk 'match($0, /^ *opam-([0-9]\.[^)]*)-([^-)]*-[^-)]*)) *echo "([^"]*)".*/, m) { print NR " " m[1] " " m[2] " " m[3]; }' shell/install.sh)
    if [[ $ARCHES -ne 0 ]] ; then
      echo '::error file=shell/install.sh::No sha512 checksums were detected?!'
      ERROR=1
    fi
    echo '::endgroup::'
  else
    echo 'No changes in shell/install.sh'
  fi
fi


###
# Check src_ext patches
###

(set +x ; echo -en "::group::check src_ext patches\r") 2>/dev/null
# Check that the lib-ext/lib-pkg patches are "simple"
make -C src_ext PATCH="busybox patch" clone
make -C src_ext PATCH="busybox patch" clone-pkg
# Check that the lib-ext/lib-pkg patches have been re-packaged
cd src_ext
../shell/re-patch.sh
if [[ $(find patches -name \*.old | wc -l) -ne 0 ]] ; then
  echo -e "[\e[31mERROR\e[0m] ../shell/re-patch.sh should be run from src_ext before CI check"
  git diff
  ERROR=1
fi
cd ..
(set +x ; echo -en "::endgroup::check src_ext patches\r") 2>/dev/null

###
# Default cli version check
###

if [ "$GITHUB_EVENT_NAME" = "push" ] && [ "$BRANCH" = "master" ]; then
  (set +x ; echo -en "::group::check default cli\r") 2>/dev/null
  CURRENT_MAJOR="`sed -n "s/^AC_INIT(opam,\([0-9]\+\)[^0-9]*.*)$/\1/p" configure.ac`"
  DEFAULT_CLI_MAJOR="`sed -n "/let *default *=/s/.*(\([0-9]*\)[^0-9]*.*/\1/p" src/client/opamCLIVersion.ml`"
  if [ $CURRENT_MAJOR -eq $DEFAULT_CLI_MAJOR ]; then
    echo "Major viersion is default cli one: $CURRENT_MAJOR"
  else
    echo -e "[\e[31mERROR\e[0m] Major version $CURRENT_MAJOR and default cli version $DEFAULT_CLI_MAJOR mismatches"
  (set +x ; echo -en "::endgroup::check default cli\r") 2>/dev/null
    ERROR=1
  fi
fi

exit $ERROR
