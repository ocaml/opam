#!/bin/bash
# This script check that the current master changelog has been updated by PR,
# ignoring some internal files.
# It is used by the changelog_check github action.

# (c) Copyright Raja Boujbel OCamlPro 2020

set -ue

IGNORE="
.gitattributes
.github
.gitignore
.ocamlinit
.ocp-indent
.ocplint
.travis-ci.sh
.travis.yml
AUTHORS
CONTRIBUTING.md
README.md
CHANGES
LICENSE
appveyor.patch
appveyor.yml
appveyor_build.cmd
appveyor_test.sh
master_changes.md
shell/install.sh
release
"
changelog=master_changes.md
diffile=/tmp/diff

git fetch origin "$GITHUB_BASE_REF" --depth=1 --quiet
echo "> base commit"
git show origin/"$GITHUB_BASE_REF" --format=oneline -s

git diff origin/"$GITHUB_BASE_REF" --name-only --diff-filter=AMRCX > $diffile
updated=0
grep -sq $changelog $diffile || updated=1

echo "> all changes"
cat $diffile

for ign in $IGNORE ; do
  sed -i "/^${ign//\//\\\/}/d" $diffile
done

echo "> kept changes"
cat $diffile

num_changes=$(wc -l $diffile | cut -f 1 -d ' ')

if [ "$num_changes" -ne 0 ] ; then
  if [ $updated -eq 0 ] ; then
    echo -e "\033[32mChangelog updated\033[m"
  else
    echo -e "\033[31mPlease update changelog in master_changes.md\033[m"
    exit 1
  fi
else
  echo -e "\033[33mCommitted files not concerned by changelog\033[m"
fi
