_opam_add()
{
  _opam_reply="$_opam_reply $1"
}

_opam_add_f()
{
  local cmd
  cmd=$1; shift
  _opam_add "$($cmd "$@" 2>/dev/null)"
}

_opam_flags()
{
  opam "$@" --help=groff 2>/dev/null | \
      sed -n \
      -e 's%\\-%-%g' \
      -e 's%^\\fB\(-[^,= ]*\)\\fR.*%\1%p'
}

_opam_commands()
{
  opam "$@" --help=groff 2>/dev/null | \
      sed -n \
      -e 's%\\-%-%g' \
      -e '/^\.SH COMMANDS$/,/^\.SH/ s%^\\fB\([^,= ]*\)\\fR.*%\1%p'
  echo '--help'
}

_opam_vars()
{
  opam config list --safe 2>/dev/null | \
      sed -n \
      -e '/^PKG:/d' \
      -e 's%^\([^# ][^ ]*\).*%\1%p'
}

_opam()
{
  local cmd subcmd cur prev compgen_opt

  COMPREPLY=()
  cmd=${COMP_WORDS[1]}
  subcmd=${COMP_WORDS[2]}
  cur=${COMP_WORDS[COMP_CWORD]}
  prev=${COMP_WORDS[COMP_CWORD-1]}
  compgen_opt=""
  _opam_reply=""

  if [ $COMP_CWORD -eq 1 ]; then
      _opam_add_f opam help topics
      COMPREPLY=( $(compgen -W "$_opam_reply" -- $cur) )
      unset _opam_reply
      return 0
  fi
  case "$cmd" in
      install|show|info)
          _opam_add_f opam list --safe -a -s
          if [ $COMP_CWORD -gt 2 ]; then
              _opam_add_f _opam_flags "$cmd"
          fi;;
      reinstall|remove|uninstall)
          _opam_add_f opam list --safe -i -s
          if [ $COMP_CWORD -gt 2 ]; then
              _opam_add_f _opam_flags "$cmd"
          fi;;
      upgrade)
          _opam_add_f opam list --safe -i -s
          _opam_add_f _opam_flags "$cmd"
          ;;
      switch)
          case $COMP_CWORD in
              2)
                  _opam_add_f _opam_commands "$cmd"
                  _opam_add_f opam switch list --safe -s -i;;
              3)
                  case "$subcmd" in
                      install|set)
                          _opam_add_f opam switch list --safe -s -a;;
                      remove|reinstall)
                          _opam_add_f opam switch list --safe -s -i;;
                      import|export)
                          compgen_opt="-o filenames -f";;
                      *)
                          _opam_add_f _opam_flags "$cmd"
                  esac;;
              *)
                  _opam_add_f _opam_flags "$cmd"
          esac;;
      config)
          if [ $COMP_CWORD -eq 2 ]; then
              _opam_add_f _opam_commands "$cmd"
          else
              if [ $COMP_CWORD -eq 3 ] && [ "$subcmd" = "var" ]; then
                  _opam_add_f _opam_vars
              else
                  _opam_add_f _opam_flags "$cmd"
              fi
          fi;;
      repository|remote)
          case $COMP_CWORD in
              2)
                  _opam_add_f _opam_commands "$cmd";;
              3)
                  case "$subcmd" in
                      add)
                          if [ $COMP_CWORD -gt 3 ]; then
                              compgen_opt="-o filenames -f"
                          fi;;
                      remove|priority|set-url)
                          _opam_add_f opam repository list --safe -s;;
                      *)
                          _opam_add_f _opam_flags "$cmd"
                  esac;;
              *)
                  _opam_add_f _opam_flags "$cmd"
                  case "$subcmd" in
                      set-url|add) compgen_opt="-o filenames -f";;
                  esac;;
          esac;;
      update)
          _opam_add_f opam repository list --safe -s
          _opam_add_f opam pin list --safe -s
          ;;
      source)
          _opam_add_f opam list --safe -A -s
          _opam_add_f _opam_flags "$cmd"
          ;;
      pin)
          case $COMP_CWORD in
              2)
                  _opam_add_f _opam_commands "$cmd";;
              3)
                  case "$subcmd" in
                      add)
                          _opam_add_f opam list --safe -A -s;;
                      remove|edit)
                          _opam_add_f opam pin list --safe -s;;
                      *)
                          _opam_add_f _opam_flags "$cmd"
                  esac;;
              *)
                  case "$subcmd" in
                      add)
                          compgen_opt="-o filenames -f";;
                      *)
                          _opam_add_f _opam_flags "$cmd"
                  esac
          esac;;
      unpin)
          if [ $COMP_CWORD -eq 2 ]; then
              _opam_add_f opam pin list --safe -s
          else
              _opam_add_f _opam_flags "$cmd"
          fi;;
      *)
          _opam_add_f _opam_commands "$cmd"
          _opam_add_f _opam_flags "$cmd"
  esac

  COMPREPLY=( $(compgen -W "$_opam_reply" $compgen_opt -- "$cur") )
  unset _opam_reply
  return 0
}

autoload bashcompinit
bashcompinit
complete -F _opam opam
