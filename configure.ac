AC_INIT(opam,2.0.0~beta3)
AC_COPYRIGHT(Copyright 2012-2017 OcamlPro SAS)

AC_CONFIG_MACRO_DIR([m4])

AC_PROG_OCAML
if test "x$OCAMLC" = "xno"; then
   AC_MSG_ERROR([You must install the OCaml compiler])
fi

AC_ARG_ENABLE([version_check],
  AS_HELP_STRING([--disable-version-check],
                 [Do not check OCaml version])
)

AC_ARG_ENABLE([checks],
  AS_HELP_STRING([--disable-checks],
                 [Assume OCaml dependencies are OK without checking])
)

# Check that OCaml version is greater or equal to 4.01.0
AS_IF([test "x${enable_version_check}" != "xno"], [
  AX_COMPARE_VERSION(
    [$OCAMLVERSION], [lt], [4.01.0],
    AC_MSG_ERROR([Your version of OCaml: $OCAMLVERSION is not supported]))
])

AC_MSG_CHECKING([for compiler os_type])
OS_TYPE=`$OCAML shell/print_config.ml os_type 2>/dev/null | fgrep -v "Cannot find" || $OCAMLC -config | fgrep os_type | sed -e "s/.*: //"`
AS_IF([ test "$?" -eq 0 ],,[AC_MSG_ERROR([failed])] )
AC_MSG_RESULT([$OS_TYPE])

# Need the C compiler used for OCaml (important on Windows, as both x86 and x64 are used)
AS_IF([test "$OCAMLBEST" = "opt"],[OCAMLBESTCC=$OCAMLOPT],[OCAMLBESTCC=$OCAMLC])
AC_PROG_CC([$($OCAMLBESTCC -config | sed -n -e "s/native_c_compiler: \(.*\) .*/\1/p") gcc cl cc])

AX_COMPARE_VERSION([$OCAMLVERSION], [ge], [4.02.0])
AC_SUBST(ocaml_4_02,"$ax_compare_version")
AX_COMPARE_VERSION([$OCAMLVERSION], [ge], [4.03.0])
AC_SUBST(ocaml_4_03,"$ax_compare_version")

AC_CHECK_TOOL([CYGPATH],[cygpath],[no])

if test x"$CYGPATH" = x"no" ; then
  AR=ar
else
  printf "include %s/Makefile.config\nall:\n\t@echo \$(ARCMD)" `$OCAMLC -where | cygpath -f -` > conftestMakefile
  AR=`make --no-print-directory -f conftestMakefile`
fi

AS_IF([ test "${OS_TYPE}" = "Win32"],[
  WIN32=1
  EXE=.exe
],[
  EXE=
  WIN32=0
])

AC_SUBST(WIN32)
AC_SUBST(EXE)

AC_MSG_CHECKING([for invocation options for $OCAMLDEP])
AS_IF([ test ${WIN32} -eq 1 ],[
  OCAMLDEPARGS=-slash
  AC_SUBST(OCAMLDEP,"$OCAMLDEP -slash")
  AC_MSG_RESULT([$OCAMLDEPARGS])
],[
  OCAMLDEPARGS=
  AC_MSG_RESULT([none required])
])
AC_SUBST(OCAMLDEPARGS)

AS_IF([ test "x$AR" != "x" ],[AC_CHECK_TOOL(AR,$AR)],[AC_SUBST(AR,"no")])

AC_PROG_OCAMLLEX
AC_PROG_OCAMLYACC
AC_PROG_FINDLIB

AC_ARG_ENABLE([certificate_check],
  AS_HELP_STRING([--disable-certificate-check],
                 [Do not check the certificate of opam's dependency archives])
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
elif test x"${enable_checks}" != x"no" ; then
  AC_MSG_ERROR([You must have either curl or wget installed.])
fi

echo

AC_CHECK_OCAML_PKG([unix])
AC_CHECK_OCAML_PKG([bigarray])
AC_CHECK_OCAML_PKG([extlib])
AC_CHECK_OCAML_PKG([re])
AC_CHECK_OCAML_PKG([re.glob])
AC_CHECK_OCAML_PKG([cmdliner])
AC_CHECK_OCAML_PKG([ocamlgraph])
AC_CHECK_OCAML_PKG([cudf])
AC_CHECK_OCAML_PKG(dose3.common,dose.common)
AC_CHECK_OCAML_PKG(dose3.algo,dose.algo)
AC_CHECK_OCAML_PKG([jsonm])
AC_CHECK_OCAML_PKG([opam-file-format])

dnl echo
dnl echo "extlib........................ ${OCAML_PKG_extlib}"
dnl echo "re............................ ${OCAML_PKG_re}"
dnl echo "cmdliner...................... ${OCAML_PKG_cmdliner}"
dnl echo "graph......................... ${OCAML_PKG_ocamlgraph}"
dnl echo "cudf.......................... ${OCAML_PKG_cudf}"
dnl echo "dose3......................... ${OCAML_PKG_dose3}"
dnl echo "jsonm......................... ${OCAML_PKG_jsonm}"
echo


AS_IF([test "x${enable_checks}" != "xno" && {
       test "x$OCAML_PKG_extlib" = "xno" ||
       test "x$OCAML_PKG_re" = "xno" ||
       test "x$OCAML_PKG_cmdliner" = "xno" ||
       test "x$OCAML_PKG_ocamlgraph" = "xno" ||
       test "x$OCAML_PKG_cudf" = "xno" ||
       test "x$OCAML_PKG_dose3_common" = "xno" ||
       test "x$OCAML_PKG_jsonm" = "xno" ||
       test "x$OCAML_PKG_opam_file_format" = "xno"; }],[
  echo "============================================================================"
  echo "Some dependencies are missing. If you are just interested in the stand-alone"
  echo "'opam' binary, run 'make lib-ext' to download and include them."
  echo "============================================================================"
  echo
  AC_SUBST(hasalldeps,"")
  printf "S src_ext/*\nB src_ext/*" > .merlin
  grep -v "^PKG" .merlin.in >> .merlin
],[
  AC_SUBST(hasalldeps,"true")
  cp .merlin.in .merlin
])

if test "x$prefix" = "xNONE"; then
  prefix=$ac_default_prefix
fi

AC_CONFIG_FILES(
  Makefile.config
  src/core/opamVersion.ml
  src/core.META
  src/format.META
  src/repository.META
  src/solver.META
  src/state.META
  src/client.META
  src/admin.META
)
AC_OUTPUT

echo

bindir="`eval echo ${bindir}`"
bindir="`eval echo ${bindir}`"
mandir="`eval echo ${mandir}`"
mandir="`eval echo ${mandir}`"

echo Executables will be installed in ${bindir}
echo Manual pages will be installed in ${mandir}
