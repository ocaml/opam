_opam_env_hook() {
    if [ ! -e .envrc ]; then eval $(opam env --shell=zsh --readonly 2> /dev/null <&-); fi;
}
typeset -ag precmd_functions;
if [[ -z ${precmd_functions[(r)_opam_env_hook]} ]]; then
    precmd_functions+=_opam_env_hook;
fi
