_opam_add()
{
  _opam_reply="$_opam_reply $1"
}

_opam_global_options()
{
  local res
  res="$( opam --help=plain 2>/dev/null | grep '^       -' | sed 's/ *//;s/[ ,\[\=].*//' )"
  _opam_add "$res"
}

_opam_commands()
{
  local res
  res="$( opam help topics )"
  _opam_add "$res"
}

_opam_flags()
{
  local res cmd
  cmd="$1"
  res="$( opam $cmd --help=plain 2>/dev/null | grep '^       -' | sed 's/ *//;s/[ ,\[\=].*//' )"
  _opam_add "$res"
}

_opam_packages()
{
  local res
  res="$( opam list -a -s )"
  _opam_add "$res"
}

_opam_installed_packages()
{
  local res
  res="$( opam list -i -s )"
  _opam_add "$res"
}

_opam_compilers()
{
  local res
  res="$( opam switch -s )"
  _opam_add "$res"
}

_opam_config_subcommands()
{
  _opam_add "env var list subst includes bytecomp asmcomp bytelink asmlink"
}

_opam_config_vars()
{
  local res
  res="$( opam config list 2>/dev/null | sed 's/ *//;s/ .*//' )"
  _opam_reply="$res"
}

_opam_repository_subcommands()
{
  _opam_add "add remove list priority"
}

_opam_repositories()
{
  local res
  res="$( opam remote -s )"
  _opam_add "$res"
}

_opam_repositories_only()
{
  _opam_reply="$( opam remote -s )"
}

_opam()
{
  local cmd cur prev

  COMPREPLY=()
  cmd=${COMP_WORDS[1]}
  subcmd=${COMP_WORDS[2]}
  cur=${COMP_WORDS[COMP_CWORD]}
  prev=${COMP_WORDS[COMP_CWORD-1]}
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
          reinstall|remove)
              _opam_installed_packages
              ;;
          switch)
              _opam_compilers
              ;;
          config)
              _opam_config_subcommands
              if [ "$prev" = "var" ]; then _opam_config_vars; fi
              ;;
          repository)
              _opam_repository_subcommands
              case "$subcmd" in
                  remove)
                      _opam_repositories_only
                      ;;
              esac
              ;;
          update)
              _opam_repositories
              ;;
      esac

  fi

  COMPREPLY=( $(compgen -W "$_opam_reply" -- $cur) )
  unset _opam_reply
  return 0
}

complete -F _opam opam
