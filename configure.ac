AC_INIT(opam,0.5)
AM_INIT_AUTOMAKE
AC_COPYRIGHT(Copyright 2012 OcamlPro SAS)

AC_CONFIG_MACRO_DIR([m4])

AC_PROG_OCAML
if test "$OCAMLC" = "no"; then
   AC_MSG_ERROR([You must install the OCaml compiler])
fi

AC_CONFIG_FILES([Makefile])
AC_OUTPUT

if test "$prefix" = "NONE"; then
  prefix=$ac_default_prefix
fi

bindir="`eval echo ${bindir}`"
bindir="`eval echo ${bindir}`"
mandir="`eval echo ${mandir}`"
mandir="`eval echo ${mandir}`"

echo
echo Executables will be installed in ${bindir}
echo Manual pages will be installed in ${mandir}