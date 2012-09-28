OPAM_EXECUTABLE_PATH=$(which opam) || return 1
function opam () {    
    "$OPAM_EXECUTABLE_PATH" "$@"
    if [ -n "$1" ] && [ "$1" = "switch" ]
    then
        eval $("$OPAM_EXECUTABLE_PATH" config -env)
    fi
} 
