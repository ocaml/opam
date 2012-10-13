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
  res="$( opam --help 2>/dev/null | grep '^  [^ ]' | sed 's/ *//;s/ .*//' | grep -v '^-' )"
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
  res="$( opam list -short )"
  _opam_add "$res"
}

_opam_compilers()
{
  local res count
  count="$( opam switch -list 2>/dev/null | grep -n '\--- Compilers available' | sed 's/\([:digit:]*\)\:.*/\1/' )"
  res="$( opam switch -list 2>/dev/null | tail -n $(($count+1)) | sed 's/^[ ~] *//' )"
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
  reprev=${COMP_WORDS[COMP_CWORD-2]}
  _opam_reply=""

  _opam_global_options

  if [ $COMP_CWORD -eq 1 ]; then
      _opam_commands
  elif [ $COMP_CWORD -gt 1 ]; then
      _opam_flags "$cmd"
      case "$cmd" in
          install|info)
              _opam_packages
              ;;
          switch)
              _opam_compilers
              ;;
          config)
              if [ "$prev" = "-var" ]; then _opam_config_vars; fi
              ;;
          remote)
              if [ "$prev" = "-add" ]; then return 0; fi
              if [ "$reprev" = "-add" ]; then _filedir -d; return 0; fi
              ;;
      esac

  fi

  COMPREPLY=( $(compgen -W "$_opam_reply" -- $cur) )
  unset _opam_reply
  return 0
}

complete -F _opam opam