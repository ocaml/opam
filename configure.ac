AC_INIT(opam,0.5)
AM_INIT_AUTOMAKE

AC_CONFIG_MACRO_DIR([m4])

AC_PROG_CC

AC_PROG_OCAML
if test "$OCAMLC" = "no"; then
   AC_MSG_ERROR([You must install the OCaml compiler])
fi

AC_PROG_FINDLIB
if test "$OCAMLFIND" = "no"; then
   AC_MSG_ERROR([You must install OCaml findlib])
fi

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
