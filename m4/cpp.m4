AC_DEFUN([CHECK_CXX_WORKS], [
  AC_MSG_CHECKING([whether $CXX really is a C++ compiler])
  AC_LANG_PUSH([C++])
  AC_COMPILE_IFELSE(
    [AC_LANG_SOURCE([
       #ifndef __cplusplus
       #error This is not a C++ compiler!
       #endif
    ])],
    [AC_MSG_RESULT([yes])],
    [AC_MSG_RESULT([no])
     CXX=''])
  AC_LANG_POP([C++])])
