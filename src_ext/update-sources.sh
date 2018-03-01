#!/bin/bash

cd $(dirname $0)
echo -n "Checking packages for new versions in opam: "
DISAGREEMENTS=()
while read name prefix version url; do
  package=$name
  if [[ $package = "findlib" ]] ; then package=ocamlfind ; fi
  latest=$(opam show $package -f all-versions)
  latest=${latest##* }
  package_url=$(opam show $package.$latest -f url.src:)
  md5=$(sed -n -e "s/MD5$prefix$name *= *\(.*\)/\1/p" Makefile.sources)
  package_md5=$(opam show $package.$latest -f url.checksum: | sed -e "s/.*md5=\([a-fA-F0-9]\{32\}\).*/\1/")
  if [[ $package_url = $url ]] ; then
    if [[ $package_md5 = $md5 ]] ; then
      echo -ne "[\033[0;32m$name\033[m] "
      if [[ $latest != $version ]] ; then
        DISAGREEMENTS+=" $name ($version vs $latest in opam)"
      fi
    else
      echo "\n$name: [\033[1;33mWARN\033[m] MD5 is wrong for (should be $package_md5 according to opam)"
    fi
  else
    if [[ $package_md5 = $md5 ]] ; then
      echo -e "\n$name: [\033[1;33mWARN\033[m] URL is wrong for $name (should be $package_url according to opam)"
    else
      if [[ $latest = $version ]] ; then
        echo -e "\n$name: [\033[1;33mWARN\033[m] URL and MD5 are wrong for $name (should be $package_url (md5=$package_md5) according to opam)"
      else
        echo -ne "[\033[0;31m$name\033[m: \033[1m$latest\033[m] "
        sed -i -e "s/\(URL$prefix$name *= *\).*/\1${package_url////\\/}/" -e "s/\(MD5$prefix$name *= *\).*/\1$package_md5/" Makefile.sources
      fi
    fi
  fi
done < <(fgrep URL_ Makefile.sources | sed -e "s/URL\(_\(PKG_\)\?\)\([^ =]*\) *= *\(.*\/\([^0-9][^-]*-\)\?v\?\)\([0-9.]\+\([-+.][^\/]*\)\?\)\(\.tbz\|\.tar\.gz\)/\3 \1 \6 \4\6\8/" | sort)
echo -e "\nComplete."
if [[ ${#DISAGREEMENTS[@]} -gt 0 ]] ; then
  echo "Disagreements over version:${DISAGREEMENTS[@]}"
fi
