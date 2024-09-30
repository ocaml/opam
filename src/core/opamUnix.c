/**************************************************************************/
/*                                                                        */
/*    Copyright 2024 Kate Deplaix                                         */
/*                                                                        */
/*  All rights reserved. This file is distributed under the terms of the  */
/*  GNU Lesser General Public License version 2.1, with the special       */
/*  exception on linking described in the file LICENSE.                   */
/*                                                                        */
/**************************************************************************/

#include <sys/ioctl.h>

CAMLprim value opam_stdout_ws_col(value _unit) {
    struct winsize win;

    if (-1 == ioctl(STDOUT_FILENO, TIOCGWINSZ, &win)) {
        return Val_int(0);
    }
    return Val_int(win.ws_col);
}

#include <sys/utsname.h>

CAMLprim value opam_uname(value _unit) {
  struct utsname buf;
  value ret;

  if (-1 == uname(&buf)) {
    caml_uerror("uname", Nothing);
  }
  ret = caml_alloc(3, 0);
  Store_field(ret, 0, caml_copy_string(buf.sysname));
  Store_field(ret, 1, caml_copy_string(buf.release));
  Store_field(ret, 2, caml_copy_string(buf.machine));

  return ret;
}
