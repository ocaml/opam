#! /bin/sh

TEST_DIR=/tmp
OPAM_ROOT=$TEST_DIR/OPAM.ROOT
OPAM_REPO=$TEST_DIR/OPAM.REPO
BIN=$TEST_DIR/OPAM.BIN
REPO=test
 
BINARIES=opam
 
# opam in the path should not be a requirement
ENV="OCAMLRUNPARAM=b OPAMDEBUG=2 OPAM_ROOT=$OPAM_ROOT PATH=$BIN:$PATH"
ENV="OCAMLRUNPARAM=b OPAM_ROOT=$OPAM_ROOT PATH=$BIN:$PATH"
OPAM="$ENV opam --yes --root $OPAM_ROOT"
 
function binaries() {
  mkdir -p $BIN
  for bin in $BINARIES; do \
    cp ../_obuild/$bin/$bin.asm $BIN/$bin ; \
  done
}

function opam_clean() {
  rm -rf $ARCHIVES
  rm -rf $OPAM_ROOT $BIN
  rm -rf $OPAM_REPO
}

function opam_init() {
  mkdir -p $OPAM_REPO
  binaries
  eval $OPAM init -no-base-packages $REPO $OPAM_REPO -kind rsync
}

function opam_upload_stage1() {

  cd packages
  eval $OPAM upload -opam P1-1.opam -descr P1-1/README -archive P1-1.tar.gz -repo $REPO
  eval $OPAM upload -opam P2.opam -descr P2/README -archive P2.tar.gz -repo $REPO
  eval $OPAM upload -opam P3.opam -descr P3/README -archive P3.tar.gz -repo $REPO
  eval $OPAM upload -opam P4-1.opam -descr P4/README -archive P4.tar.gz -repo $REPO
  eval $OPAM upload -opam P5.opam -descr P5/README -archive P5.tar.gz -repo $REPO
  cd -

  cp compilers/* $OPAM_REPO/compilers/
  # update the list of available packages with the one being updated
  eval $OPAM update 
}

function opam_upload_stage2() {

  cd packages
  eval $OPAM upload -opam P1-2.opam -descr P1-2/README -archive P1-2.tar.gz -repo $REPO
  eval $OPAM upload -opam P4-2.opam -descr P4/README -archive P4.tar.gz -repo $REPO
  eval $OPAM upload -opam P4-3.opam -descr P4/README -archive P4.tar.gz -repo $REPO
  cd -

  # update the list of available packages with the one being updated
  eval $OPAM update 
}
function usage() {
DESCRIPTION="Opam unittest init functions"
cat << EOF
usage: $0 options
 
$DESCRIPTION
 
OPTIONS:
   -h      Show this message
   -v      Verbose
   -d      Debug
   -i      Init
   -c      Clean
EOF
}
 
VERBOSE=
DEBUG=
INIT=
CLEAN=
STAGE=

while getopts "vhdcis:" flag
do
  case "$flag" in
    d) set -x ; DEBUG=true;;
    v) VERBOSE=true ;;
    i) INIT=true ;;
    s) STAGE=$OPTARG ;;
    c) CLEAN=true ;;
    h) usage ; exit 0 ;;
  esac
#  echo "$flag" $OPTIND $OPTARG
done
 
if [ -n "$INIT" ]; then
  opam_clean
  opam_init
fi

if [ -n "$STAGE" ]; then
  if [ $STAGE = "1" ]; then
    opam_upload_stage1
  fi

  if [ $STAGE = "2" ]; then
    opam_upload_stage2
  fi
fi

if [ -n "$CLEAN" ]; then
  opam_clean
fi

exit 0


