#!/bin/bash -xue

PR_COMMIT_RANGE=
if [ "$GITHUB_EVENT_NAME" = "pull_request" ]; then
  PR_COMMIT_RANGE="$GITHUB_REF_SHA...$GITHUB_SHA"
fi
CI_BRANCH=${GITHUB_REF##*/}


echo "PR_COMMIT_RANGE=$PR_COMMIT_RANGE"
echo "GITHUB_SHA=$GITHUB_SHA"
if [[ $GITHUB_EVENT_NAME = 'pull_request' ]] ; then
  FETCH_HEAD=$(git rev-parse FETCH_HEAD)
  echo "FETCH_HEAD=$FETCH_HEAD"
else
  FETCH_HEAD=$GITHUB_SHA
fi

if [[ $GITHUB_EVENT_NAME = 'push' ]] ; then
  if ! git cat-file -e "$GITHUB_SHA" 2> /dev/null ; then
    echo 'GITHUB_SHA does not exist - CI failure'
    exit 1
  fi
else
  if [[ $GITHUB_SHA != $(git rev-parse FETCH_HEAD) ]] ; then
    echo 'WARNING! Travis GITHUB_SHA and FETCH_HEAD do not agree!'
    if git cat-file -e "$GITHUB_SHA" 2> /dev/null ; then
      echo 'GITHUB_SHA exists, so going with it'
    else
      echo 'GITHUB_SHA does not exist; setting to FETCH_HEAD'
      GITHUB_SHA=$FETCH_HEAD
    fi
  fi
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
    fi
  fi
}

set +x

ERROR=0

###
# Check install.sh
###

if [ "$GITHUB_EVENT_NAME" = "pull_request" ] ; then
  CUR_HEAD=$GITHUB_REF_SHA
  PR_HEAD=$GITHUB_SHA
  DEEPEN=50
  while ! git merge-base "$CUR_HEAD" "$PR_HEAD" >& /dev/null
  do
    echo "Deepening $CI_BRANCH by $DEEPEN commits"
    git fetch origin --deepen=$DEEPEN "$CI_BRANCH"
    ((DEEPEN*=2))
  done
  MERGE_BASE=$(git merge-base "$CUR_HEAD" "$PR_HEAD")
  if ! git diff "$MERGE_BASE..$PR_HEAD" --name-only --exit-code -- shell/install.sh > /dev/null ; then
    echo "shell/install.sh updated - checking it"
    eval $(grep '^\(OPAM_BIN_URL_BASE\|DEV_VERSION\|VERSION\)=' shell/install.sh)
    echo "OPAM_BIN_URL_BASE=$OPAM_BIN_URL_BASE"
    echo "VERSION = $VERSION"
    echo "DEV_VERSION = $DEV_VERSION"
    for VERSION in $DEV_VERSION $VERSION; do
      eval $(grep '^TAG=' shell/install.sh)
      echo "TAG = $TAG"
      ARCHES=0

      while read -r key sha
      do
        ARCHES=1
        URL="$OPAM_BIN_URL_BASE$TAG/opam-$TAG-$key"
        echo "Checking $URL"
        check=$(curl -Ls "$URL" | sha512sum | cut -d' ' -f1)
        if [ "$check" = "$sha" ] ; then
          echo "Checksum as expected ($sha)"
        else
          echo -e "[\e[31mERROR\e[0m] Checksum downloaded: $check"
          echo -e "[\e[31mERROR\e[0m] Checksum install.sh: $sha"
          ERROR=1
        fi
      done < <(sed -ne "s/.*opam-$TAG-\([^)]*\).*\"\([^\"]*\)\".*/\1 \2/p" shell/install.sh)
    done
    if [ $ARCHES -eq 0 ] ; then
      echo "[\e[31mERROR\e[0m] No sha512 checksums were detected in shell/install.sh"
      echo "That can't be right..."
      ERROR=1
    fi
  fi
fi

###
# Check configure
###

if [[ -z $PR_COMMIT_RANGE ]]
then CheckConfigure "$GITHUB_SHA"
else
  if [[ $GITHUB_EVENT_NAME = 'pull_request' ]]
  then PR_COMMIT_RANGE=$MERGE_BASE..$GITHUB_SHA
  fi
  for commit in $(git rev-list "$PR_COMMIT_RANGE" --reverse)
  do
    CheckConfigure "$commit"
  done
fi

###
# Check src_ext patches
###
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
exit $ERROR
