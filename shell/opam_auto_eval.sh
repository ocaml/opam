function opam-switch-eval () {
    opam switch "$@" --no-warning
    eval $(opam config env)
}
