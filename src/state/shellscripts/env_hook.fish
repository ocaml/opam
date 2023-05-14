function __opam_env_export_eval --on-event fish_prompt;
    test -e .envrc; or eval (opam env --shell=fish --readonly 2> /dev/null);
end
