(**************************************************************************)
(*                                                                        *)
(*    Copyright 2018 MetaStack Solutions Ltd.                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Types for C stubs modules. *)

(** CONSOLE_SCREEN_BUFFER_INFO struct
    (see https://docs.microsoft.com/en-us/windows/console/console-screen-buffer-info-str)
 *)
type console_screen_buffer_info = {
  size: int * int;
    (** Width and height of the screen buffer *)
  cursorPosition: int * int;
    (** Current position of the console cursor (caret) *)
  attributes: int;
    (** Screen attributes; see https://docs.microsoft.com/en-us/windows/console/console-screen-buffers#_win32_character_attributes *)
  window: int * int * int * int;
    (** Coordinates of the upper-left and lower-right corners of the display
        window within the screen buffer *)
  maximumWindowSize: int * int;
    (** Maximum displayable size of the console for this screen buffer *)
}

(** Win32 API handles *)
type handle

(** Standard handle constants
    (see https://docs.microsoft.com/en-us/windows/console/getstdhandle) *)
type stdhandle = STD_INPUT_HANDLE | STD_OUTPUT_HANDLE | STD_ERROR_HANDLE
