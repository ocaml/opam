function __opam_env_export_eval --on-event fish_prompt;
    eval (opam env --shell=fish --readonly 2> /dev/null);
end
