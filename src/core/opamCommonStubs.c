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
#include <sys/resource.h>

#else

#include <windows.h>
#include <io.h>
#include <sysinfoapi.h>

/* mingw-w64 defines R_OK */
#ifndef R_OK
#define R_OK 4
#endif

#endif

#if OCAML_VERSION < 50000
#define caml_unix_access unix_access
#define caml_uerror uerror
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

CAMLprim value opam_nproc(value _unit) {
#ifdef _WIN32
  SYSTEM_INFO sysinfo;
  GetSystemInfo(&sysinfo);
  return caml_copy_nativeint(sysinfo.dwNumberOfProcessors);
#else
  return caml_copy_nativeint(sysconf(_SC_NPROCESSORS_ONLN));
#endif
}

CAMLprim value opam_total_ram(value _unit) {
  int64_t total_mem = 0;
#ifdef _WIN32
  MEMORYSTATUSEX statex;
  statex.dwLength = sizeof(statex);
  if (GlobalMemoryStatusEx(&statex) == 0)
    return caml_copy_int64(0);
  total_mem = statex.ullTotalPhys;
#else
  struct rlimit rlim;
  if (getrlimit(RLIMIT_AS, &rlim) == 0) {
    if (rlim.rlim_max != RLIM_INFINITY)
      total_mem = rlim.rlim_max;
    if (rlim.rlim_cur != RLIM_INFINITY)
      total_mem = rlim.rlim_cur;
  }
  if (total_mem == 0) {
    long pagesize = sysconf(_SC_PAGESIZE);
    long phys_pages = sysconf(_SC_PHYS_PAGES);
    if (pagesize == -1 || phys_pages == -1)
      return caml_copy_int64(0);
    total_mem = (int64_t)phys_pages * (int64_t)pagesize;
  }
#endif
  int64_t one_KB = 1024;
  int64_t one_MB = one_KB * one_KB;
  return caml_copy_int64(total_mem / one_MB);
}

/* This is done here as it simplifies the dune file */
#ifdef _WIN32
#include "opamInject.c"
#include "opamWindows.c"
#else
#include "opamUnix.c"
#endif
