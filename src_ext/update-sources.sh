#!/usr/bin/env bash

set -euo pipefail

function process
{
  while read -r name; do
    package=$name
    case "$package" in
      findlib) package=ocamlfind;;
      dune-local) package=dune;;
    esac
    latest=$(opam show "$package" -f all-versions | sed -e 's/  base//')
    latest=${latest##* }
    package_url=$(opam show "$package.$latest" -f url.src: | sed -e 's/"//g')
    md5=$(sed -n -e "s/MD5_$name *= *\(.*\)/\1/p" "$1")
    package_md5=$(opam show "$package.$latest" -f url.checksum: | sed -n -e "/md5/s/.*md5=\([a-fA-F0-9]\{32\}\).*/\1/p")
    if [[ -z $package_md5 ]] ; then
      echo -e "$name: [\033[1;33mWARN\033[m] no md5 given in opam, downloading $package_url to check"
      package_md5=$(curl -LSs "$package_url" | md5sum | cut -f1 -d' ')
    fi
    if [[ $package_md5 = "$md5" ]] ; then
      echo -e "$name: [\033[1;32mNOTE\033[m] $name is up-to-date"
    else
      echo -e "[\033[0;31m$name\033[m: \033[1m$latest\033[m] is being updated"
      sed -e "s/\(URL_$name *= *\).*/\1${package_url////\\/}/" -e "s/\(MD5_$name *= *\).*/\1$package_md5/" "$1" > "$1.tmp"
      mv "$1.tmp" "$1"
    fi
  done < <(grep -F URL_ "$1" | sed -e "s/URL_\([^ =]*\) *=.*/\1/" | sort)
}

cd "$(dirname "$0")"
echo "Checking packages for new versions in opam:"
process Makefile.sources
process Makefile.dune
echo "Complete."
