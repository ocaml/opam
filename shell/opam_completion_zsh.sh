#compdef opam

if [ -z "$ZSH_VERSION" ]; then return 0; fi

_opam_add()
{
  IFS=$'\n' _opam_reply+=("$@")
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
      -e 's%\\-\|\\N'"'45'"'%-%g' \
      -e 's%, \\fB%\n\\fB%g' \
      -e '/^\\fB-/p' | \
      sed -e 's%^\\fB\(-[^\\]*\).*%\1%'
}

_opam_commands()
{
  opam "$@" --help=groff 2>/dev/null | \
      sed -n \
      -e 's%\\-\|\\N'"'45'"'%-%g' \
      -e '/^\.SH COMMANDS$/,/^\.SH/ s%^\\fB\([^,= ]*\)\\fR.*%\1%p'
  echo '--help'
}

_opam_vars()
{
  opam config list --safe 2>/dev/null | \
      sed -n \
      -e '/^PKG:/d' \
      -e 's%^\([^#= ][^ ]*\).*%\1%p'
}

_opam_argtype()
{
  local cmd flag
  cmd="$1"; shift
  flag="$1"; shift
  case "$flag" in
      -*)
          opam "$cmd" --help=groff 2>/dev/null | \
          sed -n \
              -e 's%\\-\|\\N'"'45'"'%-%g' \
              -e 's%.*\\fB'"$flag"'\\fR[= ]\\fI\([^, ]*\)\\fR.*%\1%p'
          ;;
  esac
}

_opam()
{
  local IFS cmd subcmd cur prev compgen_opt

  COMPREPLY=()
  cmd=${COMP_WORDS[1]}
  subcmd=${COMP_WORDS[2]}
  cur=${COMP_WORDS[COMP_CWORD]}
  prev=${COMP_WORDS[COMP_CWORD-1]}
  compgen_opt=()
  _opam_reply=()

  if [ $COMP_CWORD -eq 1 ]; then
      _opam_add_f opam help topics
      COMPREPLY=( $(compgen -W "${_opam_reply[*]}" -- $cur) )
      unset _opam_reply
      return 0
  fi

  case "$(_opam_argtype $cmd $prev)" in
      LEVEL|JOBS|RANK) _opam_add 1 2 3 4 5 6 7 8 9;;
      FILE|FILENAME) compgen_opt+=(-o filenames -f);;
      DIR|ROOT) compgen_opt+=(-o filenames -d);;
      MAKE|CMD) compgen_opt+=(-c);;
      KIND) _opam_add http local git darcs hg;;
      WHEN) _opam_add always never auto;;
      SWITCH|SWITCHES) _opam_add_f opam switch list --safe -s;;
      COLUMNS|FIELDS)
          _opam_add name version package synopsis synopsis-or-target \
                    description installed-version pin source-hash \
                    opam-file all-installed-versions available-versions \
                    all-versions repository installed-files vc-ref depexts;;
      PACKAGE|PACKAGES|PKG|PATTERN|PATTERNS)
          _opam_add_f opam list --safe -A -s;;
      FLAG) _opam_add light-uninstall verbose plugin compiler conf;;
      REPOS) _opam_add_f opam repository list --safe -s -a;;
      SHELL) _opam_add bash sh csh zsh fish;;
      TAGS) ;;
      CRITERIA) ;;
      STRING) ;;
      URL)
          compgen_opt+=(-o filenames -d)
          _opam_add "https://" "http://" "file://" \
                    "git://" "git+file://" "git+ssh://" "git+https://" \
                    "hg+file://" "hg+ssh://" "hg+https://" \
                    "darcs+file://" "darcs+ssh://" "darcs+https://";;
      "")
  case "$cmd" in
      install|show|info|inst|ins|in|i|inf|sh)
          _opam_add_f opam list --safe -a -s
          if [ $COMP_CWORD -gt 2 ]; then
              _opam_add_f _opam_flags "$cmd"
          fi;;
      reinstall|remove|uninstall|reinst|remov|uninst|unins)
          _opam_add_f opam list --safe -i -s
          if [ $COMP_CWORD -gt 2 ]; then
              _opam_add_f _opam_flags "$cmd"
          fi;;
      upgrade|upg)
          _opam_add_f opam list --safe -i -s
          _opam_add_f _opam_flags "$cmd"
          ;;
      switch|sw)
          case $COMP_CWORD in
              2)
                  _opam_add_f _opam_commands "$cmd"
                  _opam_add_f opam switch list --safe -s;;
              3)
                  case "$subcmd" in
                      create|install)
                          _opam_add_f opam switch list-available --safe -s -a;;
                      set|remove|reinstall)
                          _opam_add_f opam switch list --safe -s;;
                      import|export)
                          compgen_opt+=(-o filenames -f);;
                      *)
                          _opam_add_f _opam_flags "$cmd"
                  esac;;
              *)
                  _opam_add_f _opam_flags "$cmd"
          esac;;
      config|conf|c)
          case $COMP_CWORD in
              2)
                  _opam_add_f _opam_commands "$cmd";;
              3)
                  case "$subcmd" in
                      var) _opam_add_f _opam_vars;;
                      exec) compgen_opt+=(-c);;
                      *) _opam_add_f _opam_flags "$cmd"
                  esac;;
              *)
                  _opam_add_f _opam_flags "$cmd"
          esac;;
      repository|remote|repos|repo)
          case $COMP_CWORD in
              2)
                  _opam_add_f _opam_commands "$cmd";;
              3)
                  case "$subcmd" in
                      list)
                          _opam_add_f _opam_flags "$cmd";;
                      *)
                          _opam_add_f opam repository list --safe -a -s
                  esac;;
              *)
                  _opam_add_f _opam_flags "$cmd"
                  case "$subcmd" in
                      set-url|add) compgen_opt+=(-o filenames -f);;
                      set-repos) _opam_add_f opam repository list --safe -a -s;;
                  esac;;
          esac;;
      update|upd)
          _opam_add_f opam repository list --safe -s
          _opam_add_f opam pin list --safe -s
          _opam_add_f _opam_flags "$cmd"
          ;;
      source|so)
          if [ $COMP_CWORD -eq 2 ]; then
              _opam_add_f opam list --safe -A -s
          else
              _opam_add_f _opam_flags "$cmd"
          fi;;
      pin)
          case $COMP_CWORD in
              2)
                  _opam_add_f _opam_commands "$cmd";;
              3)
                  case "$subcmd" in
                      add)
                          compgen_opt+=(-o filenames -d)
                          _opam_add_f opam list --safe -A -s;;
                      remove|edit)
                          _opam_add_f opam pin list --safe -s;;
                      *)
                          _opam_add_f _opam_flags "$cmd"
                  esac;;
              *)
                  case "$subcmd" in
                      add)
                          compgen_opt+=(-o filenames -d);;
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
      var|v)
          if [ $COMP_CWORD -eq 2 ]; then _opam_add_f _opam_vars
          else _opam_add_f _opam_flags "$cmd"; fi;;
      exec|e)
          if [ $COMP_CWORD -eq 2 ]; then compgen_opt+=(-c)
          else _opam_add_f _opam_flags "$cmd"; fi;;
      lint|build)
          if [ $COMP_CWORD -eq 2 ]; then
              compgen_opt+=(-f -X '!*opam' -o plusdirs)
          else _opam_add_f _opam_flags "$cmd"; fi;;
      admin)
          if [ $COMP_CWORD -eq 2 ]; then
              _opam_add_f _opam_commands "$cmd"
          else _opam_add_f _opam_flags "$cmd" "$subcmd"; fi;;
      *)
          _opam_add_f _opam_commands "$cmd"
          _opam_add_f _opam_flags "$cmd"
  esac;;
  esac

  COMPREPLY=($(compgen -W "${_opam_reply[*]}" "${compgen_opt[@]}" -- "$cur"))
  unset _opam_reply
  return 0
}

autoload bashcompinit
bashcompinit
complete -F _opam opam
