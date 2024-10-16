/**************************************************************************/
/*                                                                        */
/*    Copyright 2024 Kate Deplaix                                         */
/*                                                                        */
/*  All rights reserved. This file is distributed under the terms of the  */
/*  GNU Lesser General Public License version 2.1, with the special       */
/*  exception on linking described in the file LICENSE.                   */
/*                                                                        */
/**************************************************************************/

/* Needed for the Windows string conversion functions on older OCaml */
#define CAML_INTERNALS

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/signals.h>
#include <caml/osdeps.h>
#include <caml/unixsupport.h>
#include <caml/version.h>

#ifndef _WIN32

#include <fcntl.h>
#include <unistd.h>

#else

#include <io.h>

/* mingw-w64 defines R_OK */
#ifndef R_OK
#define R_OK 4
#endif

#endif

#if OCAML_VERSION < 50000
#define caml_unix_access unix_access
#endif

CAMLprim value opam_is_executable(value path)
{
  CAMLparam1(path);
  char_os * p;
  int ret;

  caml_unix_check_path(path, "faccessat");
  p = caml_stat_strdup_to_os(String_val(path));
  caml_enter_blocking_section();
#ifdef _WIN32
  /* No execute bit on Windows */
  ret = _waccess(p, R_OK);
#else
  ret = faccessat(AT_FDCWD, p, X_OK, AT_EACCESS);
#endif
  caml_leave_blocking_section();
  caml_stat_free(p);
  CAMLreturn(Val_bool(ret == 0));
}

/* This is done here as it simplifies the dune file */
#ifdef _WIN32
#include "opamInject.c"
#include "opamWindows.c"
#else
#include "opamUnix.c"
#endif
