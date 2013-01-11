_opam_add()
{
  _opam_reply="$_opam_reply $1"
}

_opam_global_options()
{
  local res
  res="$( opam --help 2>/dev/null | grep '^  -' | sed 's/ *//;s/ .*//' )"
  _opam_add "$res"
}

_opam_commands()
{
  local res
  res="$( opam help topics 2>/dev/null | sed 's/ *//;s/ .*//' | grep -v '^-' )"
  _opam_add "$res"
}

_opam_flags()
{
  local res cmd
  cmd="$1"
  res="$( opam $cmd --help 2>/dev/null | grep '^  -' | sed 's/ *//;s/ .*//' | grep -v '^--' )"
  _opam_add "$res"
}

_opam_packages()
{
  local res
  res="$( opam list --short )"
  _opam_add "$res"
}

_opam_installed_packages()
{
  local res
  res="$( opam list --short --installed )"
  _opam_add "$res"
}

_opam_compilers()
{
  local res count
  res="$( opam switch -s 2>/dev/null | sed 's/^[ ~] *//' )"
  _opam_add "$res"
}

_opam_config_vars()
{
  local res
  res="$( opam config -list-vars 2>/dev/null | sed 's/ *//;s/ .*//' )"
  _opam_reply="$res"
}

_opam()
{
  local cmd cur prev

  COMPREPLY=()
  cmd=${COMP_WORDS[1]}
  cur=${COMP_WORDS[COMP_CWORD]}
  prev=${COMP_WORDS[COMP_CWORD-1]}
  _opam_reply=""

  _opam_global_options

  if [ $COMP_CWORD -eq 1 ]; then
      _opam_commands
  elif [ $COMP_CWORD -gt 1 ]; then
      _opam_flags "$cmd"
      case "$cmd" in
          install)
              _opam_packages
              ;;
          remove)
              _opam_installed_packages
              ;;
          switch)
              _opam_compilers
              ;;
          config)
              if [ "$prev" = "-var" ]; then _opam_config_vars; fi
              ;;
      esac

  fi

  COMPREPLY=( $(compgen -W "$_opam_reply" -- $cur) )
  unset _opam_reply
  return 0
}

autoload bashcompinit
bashcompinit
complete -F _opam opam