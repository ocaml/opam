_opam_env_hook() {
 local previous_exit_status=$?;
 if [ ! -e .envrc ]; then eval $(opam env --shell=bash --readonly 2> /dev/null <&- ); fi;
 return $previous_exit_status;
};
if ! [[ "$PROMPT_COMMAND" =~ _opam_env_hook ]]; then
    PROMPT_COMMAND="_opam_env_hook;$PROMPT_COMMAND";
fi
