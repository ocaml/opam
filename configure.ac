AC_INIT(opam,1.0.0)
AC_COPYRIGHT(Copyright 2012 OcamlPro SAS)

AC_CONFIG_MACRO_DIR([m4])
AC_PROG_CC

AC_PROG_OCAML
if test "$OCAMLC" = "no"; then
   AC_MSG_ERROR([You must install the OCaml compiler])
fi
AC_PROG_CAMLP4
if test "$CAMLP4" = "no"; then
   AC_MSG_ERROR([You must install the Camlp4 pre-processor. On some operating systems, these are separate packages from the main OCaml compiler, such as camlp4-extra on Debian.])
fi

AC_ARG_ENABLE(version-check,
 [  --disable-version-check            do not check OCaml version],
 [VERSION_CHECK="$enableval"],
 [VERSION_CHECK="yes"])

# Check that OCaml version is greater or equal to 3.12.1
if test "$VERSION_CHECK" = "yes" ; then
   AX_COMPARE_VERSION( [$OCAMLVERSION], [lt], [3.12.1],
     AC_MSG_ERROR([Your version of OCaml: $OCAMLVERSION is not supported]))
fi

AC_CHECK_PROGS(FETCH,[curl wget],no)

if test x"$FETCH" = x"curl" ; then
  AC_SUBST(fetch, "curl --insecure -OL")
elif test x"$FETCH" = x"wget" ; then
  AC_SUBST(fetch, "wget --no-check-certificate")
else
  AC_MSG_ERROR([You must have either curl or wget installed.])
fi

AC_CONFIG_FILES(
  Makefile.config
  src/core/opamVersion.ml
)
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
