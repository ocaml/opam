. .github/scripts/common/preamble.sh

CWD=$PWD
if [ "$RUNNER_OS" = Windows ]; then
  CACHE="$(cygpath 'D:\Cache')"
else
  CACHE=~/.cache
  CACHE=$(eval echo $CACHE)
fi
echo "Cache -> $CACHE"
OCAML_LOCAL=$CACHE/ocaml-local
OPAM_LOCAL=$CACHE/opam-local
PATH=$OPAM_LOCAL/bin:$OCAML_LOCAL/bin:$PATH; export PATH

OPAM_COLD=${OPAM_COLD:-0}
OPAM_TEST=${OPAM_TEST:-0}
OPAM_UPGRADE=${OPAM_UPGRADE:-0}

OPAM_REPO_MAIN=https://github.com/ocaml/opam-repository.git

OPAM12CACHE=`eval echo $OPAM12CACHE`
OPAMBSROOT=`eval echo $OPAMBSROOT`

OPAMBSSWITCH=opam-build

export OPAMCONFIRMLEVEL=unsafe-yes

git config --global user.email "gha@example.com"
git config --global user.name "Github Actions CI"
git config --global gc.autoDetach false
git config --global init.defaultBranch thisShouldNotHappen

if [ -d ~/opam-repository ]; then
  OPAM_REPO_CACHE=file://$HOME/opam-repository
else
  OPAM_REPO_CACHE=$OPAM_REPO_MAIN
fi

# used only for TEST jobs
init-bootstrap () {
  if [ "$OPAM_TEST" = "1" ] || [ -n "$SOLVER" ]; then
    set -e
    export OPAMROOT=$OPAMBSROOT
    # The system compiler will be picked up
    if [ "${OPAM_REPO%.git}" != "${OPAM_REPO_MAIN%.git}" ]; then
      opam init --no-setup git+$OPAM_REPO_MAIN#$OPAM_REPO_SHA
    else
      opam init --no-setup git+$OPAM_REPO_CACHE#$OPAM_REPO_SHA
    fi

    cat >> $OPAMROOT/config <<EOF
archive-mirrors: "https://opam.ocaml.org/cache"
EOF

    eval $(opam env)
#    opam update
    CURRENT_SWITCH=$(opam var switch)
    if [[ $CURRENT_SWITCH != "default" ]] ; then
      opam switch default
      eval $(opam env)
      opam switch remove $CURRENT_SWITCH
    fi

    opam switch create $OPAMBSSWITCH ocaml-system
    eval $(opam env)
    # extlib is installed, since UChar.cmi causes problems with the search
    # order. See also the removal of uChar and uTF8 in src_ext/jbuild-extlib-src
    opam install . --deps-only

    rm -f "$OPAMBSROOT"/log/*
  fi
}
