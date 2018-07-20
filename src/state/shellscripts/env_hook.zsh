_opam_env_hook() {
    eval $(opam env --shell=zsh --readonly 2> /dev/null);
}
typeset -ag precmd_functions;
if [[ -z ${precmd_functions[(r)_opam_env_hook]} ]]; then
    precmd_functions+=_opam_env_hook;
fi
