#!/usr/bin/env bash

cd $(dirname $0)
echo -n "Checking packages for new versions in opam: "
DISAGREEMENTS=()
while read name prefix version url; do
  package=$name
  case "$package" in
    findlib) package=ocamlfind;;
    dune-local) package=dune;;
  esac
  latest=$(opam show $package -f all-versions | sed -e 's/  base//')
  latest=${latest##* }
  package_url=$(opam show $package.$latest -f url.src: | sed -e 's/"//g')
  md5=$(sed -n -e "s/MD5$prefix$name *= *\(.*\)/\1/p" Makefile.sources)
  package_md5=$(opam show $package.$latest -f url.checksum: | sed -n -e "/md5/s/.*md5=\([a-fA-F0-9]\{32\}\).*/\1/p")
  if [[ -z $package_md5 ]] ; then
    echo -e "\n$name: [\033[1;33mWARN\033[m] no md5 given in opam, downloading $package_url to check"
    package_md5=$(curl -LSs $package_url | md5sum | cut -f1 -d' ')
  fi
  if [[ $package_url = $url ]] ; then
    if [[ $package_md5 = $md5 ]] ; then
      echo -ne "[\033[0;32m$name\033[m] "
      if [[ $latest != $version ]] ; then
        DISAGREEMENTS+=" $name ($version vs $latest in opam)"
      fi
    else
      echo -e "\n$name: [\033[1;33mWARN\033[m] MD5 is wrong for (should be $package_md5 according to opam)"
    fi
  else
    if [[ $package_md5 = $md5 ]] ; then
      echo -e "\n$name: [\033[1;33mWARN\033[m] URL is wrong for $name (should be $package_url according to opam)"
    else
      if [[ $latest = $version ]] ; then
        echo -e "\n$name: [\033[1;33mWARN\033[m] URL and MD5 are wrong for $name (should be $package_url (md5=$package_md5) according to opam)"
      else
        echo -ne "[\033[0;31m$name\033[m: \033[1m$latest\033[m] "
        sed -e "s/\(URL$prefix$name *= *\).*/\1${package_url////\\/}/" -e "s/\(MD5$prefix$name *= *\).*/\1$package_md5/" Makefile.sources > Makefile.sources.tmp
        mv Makefile.sources.tmp Makefile.sources
      fi
    fi
  fi
done < <(grep -F URL_ Makefile.sources | sed -e "s/URL\(_\(PKG_\)\?\)\([^ =]*\) *= *\(.*\/\(\([^0-9][^-]*\)*-\)\?v\?\)\([0-9.]\+\([-+.][^\/]*\)\?\)\(\.tbz\|\.tar\.gz\)/\3 \1 \7 \4\7\9/" | sort)
echo -e "\nComplete."
if [[ ${#DISAGREEMENTS[@]} -gt 0 ]] ; then
  echo "Disagreements over version:${DISAGREEMENTS[@]}"
fi
