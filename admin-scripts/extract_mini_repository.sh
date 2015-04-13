#! /bin/sh

set -e

if [ $# = 0 ]; then
   cat <<EOF

This script is able to generate a minimalistic and self-contained
opam-repository for a given set of packages and compiler versions.
The resulting repository will be in './opam-mini-repository.tar.gz'

Example:

 ## Including only the latest compiler version:
 $0 ocp-indent lwt

 ## Including a specific version of OCaml or multiple versions.
 $0 ocaml.4.02.1 ocaml.4.01.0 ocp-indent lwt

Then, the resulting repository can be used to replace the default opam
repository on computer without internet access and with only few spare
disk space.

  tar xf opam-mini-repository.tar.gz
  opam init --root /tmp/opam default ./opam-mini-repository --comp 4.02.1

EOF
   exit 1
fi

DEFAULT_COMPILER=4.02.1
OPAM_REPO=https://github.com/ocaml/opam-repository/archive/master.tar.gz
REPO_DIR_NAME=opam-mini-repository

EXE_PATH=$(readlink -f "$0")
SOURCE_DIR=$(dirname "${EXE_PATH}")
TARGET_DIR=$(pwd)
WORK_DIR=$(mktemp -d)

REQUEST=$*
COMPILERS=

for package in ${REQUEST}; do
    case ${package} in
        ocaml.*)
            COMPILERS="${COMPILERS} ${package#ocaml.}"
            ;;
        *)
            PACKAGES="${PACKAGES} ${package}"
            ;;
    esac
done

if [ -z "${COMPILERS}" ]; then
    COMPILERS="${DEFAULT_COMPILER}"
fi

cleanup() {
    rm -r "${WORK_DIR:?}"
}

trap cleanup EXIT

## Fetch the opam-repository

cd "${WORK_DIR}"
wget ${OPAM_REPO}
tar xf master.tar.gz
mv opam-repository-master ${REPO_DIR_NAME}

cd ${REPO_DIR_NAME}
rm -rf "${WORK_DIR}/${REPO_DIR_NAME}/.git"

## Remove the unrequired compilers version

unrequired_compiler() {
  for version in ${COMPILERS}; do
      if [ ${version} = "$1" ]; then return 1; fi
  done
  return 0
}

for dir in compilers/*/*
do
    if unrequired_compiler "${dir##compilers/*/}"; then
        rm -r "${dir}"
    fi
done

# Remove empty directories in "compilers/"

for dir in compilers/*
do
   rmdir "${dir}" 2> /dev/null || true
done

## Convert the required compilers as packages

"${SOURCE_DIR}"/compilers-to-packages.ml

## Fetch the packages and compilers archives

for version in ${COMPILERS}; do
    opam admin make --resolve --compiler ${version} ocaml.${version} ${PACKAGES}
done

## Remove the unrequired package "versions

unrequired_version() {
    case "$1" in
        base-*)
            return 1;;
        *)
            for version in archives/*
            do
                if [ "${version}" = "archives/$1+opam.tar.gz" ]; then return 1; fi
            done
    esac
    return 0
}

for dir in packages/*/*
do
    if unrequired_version "${dir##packages/*/}"; then
        rm -r "${dir}"
    fi
done

# Remove empty directories in "packages/"

for dir in packages/*
do
   rmdir "${dir}" 2> /dev/null || true
done

## Remove unrequired files

rm -f .gitignore .travis-ci-install.sh .travis-ci.sh .travis.yml README.md

## Build the archive

cd "${WORK_DIR}"
tar czf "${TARGET_DIR}/opam-mini-repository.tar.gz" ${REPO_DIR_NAME}
