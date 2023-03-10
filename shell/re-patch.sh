#!/usr/bin/env bash
set -e

if [[ ! -d patches ]] ; then
  if [[ ! -e Makefile.sources ]] ; then
    echo 'Run this script from the src_ext directory'>&2
    exit 1
  else
    exit 0
  fi
fi

export LC_ALL=C

# Get list of packages which are patched (either mode)
for package in $(cd patches ; find . -maxdepth 1 -mindepth 1 -type d | sed -e 's|./||' -e 's/\..*//' | sort | uniq) ; do
  package=${package#./}

  # Determine which directories contain patches and which modes to test (lib-pkg/lib-ext)
  locs=''
  if [[ -d patches/$package.common ]] ; then
    locs="../patches/$package.common/*.patch"
    modes="stamp pkgstamp"
  fi
  if [[ -d patches/$package ]] ; then
    modes=${modes:-stamp}
    libext_locs="../patches/$package/*.patch"
  else
    libext_locs="$locs"
  fi
  if [[ -d patches/$package.pkg ]] ; then
    if [[ -z $locs ]] ; then modes="$modes pkgstamp" ; fi
    libpkg_locs="../patches/$package.pkg/*.patch"
  else
    libpkg_locs="$locs"
  fi

  # Re-do the patches
  for mode in $modes ; do
    # Extract the package in this mode
    make $package.$mode
    rm -rf $package
    if [[ -e $package.tar.gz ]] ; then
      tarball=$package.tar.gz
      tar_flag=z
    else
      tarball=$package.tbz
      tar_flag=j
    fi
    rm -rf tmp-$package.$mode
    mkdir tmp-$package.$mode
    cd tmp-$package.$mode
    tar -x$tar_flag -f ../$tarball
    while IFS= read -r -d '' dir ; do
      if [[ $dir = "." || $dir = ".." ]] ; then continue ; fi
      mv "$dir" a
      break
    done < <(find . -type d -print0)
    if [[ $mode = 'stamp' ]] ; then
      locs="$libext_locs"
    else
      locs="$libpkg_locs"
    fi

    # Re-do the patches in sequence
    for patch in $locs ; do
      cp -a a/ b/
      cd b
      patch -p1 --set-time --no-backup-if-mismatch < ../$patch
      cd ..
      (TZ=UTC+0 diff -Naur a b || true) > $patch.new
      if ! diff -q $patch $patch.new &>/dev/null ; then
        mv $patch $patch.old
        mv $patch.new $patch
      else
        rm $patch.new
      fi
      rm -rf a
      mv b a
    done

    cd ..
    rm -rf tmp-$package.$mode
  done
done
