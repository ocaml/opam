/**************************************************************************/
/*                                                                        */
/*    Copyright 2015, 2016, 2017, 2018 MetaStack Solutions Ltd.           */
/*                                                                        */
/*  All rights reserved. This file is distributed under the terms of the  */
/*  GNU Lesser General Public License version 2.1, with the special       */
/*  exception on linking described in the file LICENSE.                   */
/*                                                                        */
/**************************************************************************/

#define CAML_NAME_SPACE
#include <stdio.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/alloc.h>

/* In a previous incarnation, dummy C stubs were generated for non-Windows
 * builds. Although this is no longer used, the C sources retain the ability to
 * be compiled this way. */
#ifdef _WIN32

#include <Windows.h>

#define OPAMreturn CAMLreturn

#else

#define OPAMreturn(v) CAMLreturn(Val_unit)

#endif

/* Actual primitives from here */
CAMLprim value OPAMW_GetCurrentProcessID(value unit)
{
  CAMLparam1(unit);

  OPAMreturn(Val_int(GetCurrentProcessId()));
}
