$env.config = ($env.config | upsert hooks.pre_prompt {
    default [] | append {
        load-env (opam env --shell=nu | from json)
    }
})
