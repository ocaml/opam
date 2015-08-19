AC_INIT(opam,1.3.0~dev)
AC_COPYRIGHT(Copyright 2012-2015 OcamlPro SAS)

AC_CONFIG_MACRO_DIR([m4])
AC_PROG_CC

AC_PROG_OCAML
if test "$OCAMLC" = "no"; then
   AC_MSG_ERROR([You must install the OCaml compiler])
fi

AC_ARG_ENABLE([version_check],
  AS_HELP_STRING([--disable-version-check],
                 [Do not check OCaml version])
)

# Check that OCaml version is greater or equal to 3.12.1
AS_IF([test "x${enable_version_check}" != "xno"], [
  AX_COMPARE_VERSION(
    [$OCAMLVERSION], [lt], [3.12.1],
    AC_MSG_ERROR([Your version of OCaml: $OCAMLVERSION is not supported]))
])

AX_COMPARE_VERSION([$OCAMLVERSION], [ge], [4.00.0])
AC_SUBST(ocaml_4,"$ax_compare_version")
AX_COMPARE_VERSION([$OCAMLVERSION], [ge], [4.01.0])
AC_SUBST(ocaml_4_01,"$ax_compare_version")
AX_COMPARE_VERSION([$OCAMLVERSION], [ge], [4.02.0])
AC_SUBST(ocaml_4_02,"$ax_compare_version")

AC_PROG_OCAMLLEX
AC_PROG_OCAMLYACC
AC_PROG_FINDLIB

AC_ARG_ENABLE([certificate_check],
  AS_HELP_STRING([--disable-certificate-check],
                 [Do not check the certificate of OPAM's dependency archives])
)

AS_IF([test "x${enable_certificate_check}" = "xno"], [
  curl_certificate_check=--insecure
  wget_certificate_check=--no-check-certificate
])

AC_CHECK_PROGS(FETCH,[curl wget],no)

if test x"$FETCH" = x"curl" ; then
  AC_SUBST(fetch, "curl $curl_certificate_check -OL")
elif test x"$FETCH" = x"wget" ; then
  AC_SUBST(fetch, "wget $wget_certificate_check")
else
  AC_MSG_ERROR([You must have either curl or wget installed.])
fi

echo

AC_CHECK_OCAML_PKG([unix])
AC_CHECK_OCAML_PKG([extlib])
AC_CHECK_OCAML_PKG([re])
AC_CHECK_OCAML_PKG([re.emacs])
AC_CHECK_OCAML_PKG([re.str])
AC_CHECK_OCAML_PKG([re.perl])
AC_CHECK_OCAML_PKG([re.pcre])
AC_CHECK_OCAML_PKG([re.glob])
AC_CHECK_OCAML_PKG([cmdliner])
AC_CHECK_OCAML_PKG([ocamlgraph])
AC_CHECK_OCAML_PKG([cudf])
AC_CHECK_OCAML_PKG(dose3.common,dose.common)
AC_CHECK_OCAML_PKG(dose3.algo,dose.algo)
AC_CHECK_OCAML_PKG([jsonm])

dnl echo
dnl echo "extlib........................ ${OCAML_PKG_extlib}"
dnl echo "re............................ ${OCAML_PKG_re}"
dnl echo "cmdliner...................... ${OCAML_PKG_cmdliner}"
dnl echo "graph......................... ${OCAML_PKG_ocamlgraph}"
dnl echo "cudf.......................... ${OCAML_PKG_cudf}"
dnl echo "dose3......................... ${OCAML_PKG_dose3}"
dnl echo "jsonm......................... ${OCAML_PKG_jsonm}"
echo

if test "$OCAML_PKG_extlib" = "no" ||
   test "$OCAML_PKG_re" = "no" ||
   test "$OCAML_PKG_cmdliner" = "no" ||
   test "$OCAML_PKG_ocamlgraph" = "no" ||
   test "$OCAML_PKG_cudf" = "no" ||
   test "$OCAML_PKG_dose3" = "no" ||
   test "$OCAML_PKG_jsonm" = "no"; then
  echo "============================================================================"
  echo "Some dependencies are missing. If you are just interested in the stand-alone"
  echo "binaries, run 'make lib-ext' to download and include them."
  echo "============================================================================"
  echo
  AC_SUBST(hasalldeps,"")
else
  AC_SUBST(hasalldeps,"true")
fi

if test "$prefix" = "NONE"; then
  prefix=$ac_default_prefix
fi

AC_CONFIG_FILES(
  Makefile.config
  META
  src/core/opamVersion.ml
)
AC_OUTPUT

echo

bindir="`eval echo ${bindir}`"
bindir="`eval echo ${bindir}`"
mandir="`eval echo ${mandir}`"
mandir="`eval echo ${mandir}`"

echo Executables will be installed in ${bindir}
echo Manual pages will be installed in ${mandir}
