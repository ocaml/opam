#!/bin/bash

set -xue

. .github/scripts/common/hygiene-preamble.sh

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


exit $ERROR
