# This script allows you to see the active opam switch in your prompt. It
# should be portable across all shells in common use.
#
# To enable, change your PS1 to call _opam_ps1 using command substitution. For
# example, in bash:
#
#     PS1="$(__opam_ps1 "(%s)")\u@\h:\w\$ "
#

__opam_ps1()
{
    local exit=$?
    local printf_format='(%s)'

    case "$#" in
        0|1)    printf_format="${1:-$printf_format}"
        ;;
        *)  return $exit
        ;;
    esac

    local switch_name="$(opam switch show --safe 2>/dev/null)"
    if [ -z "$switch_name" ]; then
        return $exit
    fi
    printf -- "$printf_format" "$switch_name"
    return $exit
}
