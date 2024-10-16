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
