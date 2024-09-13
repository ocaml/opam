(**************************************************************************)
(*                                                                        *)
(*    Copyright 2018 MetaStack Solutions Ltd.                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Types for C stubs modules and common C stubs. *)

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

(** CONSOLE_FONT_INFOEX struct
    (see https://docs.microsoft.com/en-us/windows/console/console-font-infoex)
 *)
type console_font_infoex = {
  font: int;
    (** Index in the system's console font table *)
  fontSize: int * int;
    (** Size, in logical units, of the font *)
  fontFamily: int;
    (** Font pitch and family (low 8 bits only).
        See tmPitchAndFamily in
        https://msdn.microsoft.com/library/windows/desktop/dd145132 *)
  fontWeight: int;
    (** Font weight. Normal = 400; Bold = 700 *)
  faceName: string;
    (** Name of the typeface *)
}

(** Win32 API handles *)
type handle

(** Standard handle constants
    (see https://docs.microsoft.com/en-us/windows/console/getstdhandle) *)
type stdhandle = STD_INPUT_HANDLE | STD_OUTPUT_HANDLE | STD_ERROR_HANDLE


(** Win32 Root Registry Hives (see
    https://msdn.microsoft.com/en-us/library/windows/desktop/ms724836.aspx) *)
type registry_root =
| HKEY_CLASSES_ROOT
| HKEY_CURRENT_CONFIG
| HKEY_CURRENT_USER
| HKEY_LOCAL_MACHINE
| HKEY_USERS

(** Win32 Registry Value Types (see
    https://msdn.microsoft.com/en-us/library/windows/desktop/ms724884.aspx *)
type _ registry_value =
| REG_SZ : string registry_value

(** Windows Messages (at least, one of them!) *)
type ('a, 'b, 'c) winmessage =
| WM_SETTINGCHANGE : (int, string, int) winmessage
  (** See https://msdn.microsoft.com/en-us/library/windows/desktop/ms725497.aspx *)

(** Windows CPU Architectures (SYSTEM_INFO.wProcessArchitecture / sysinfoapi.h) *)
type windows_cpu_architecture =
| AMD64   (* 0x9 *)
| ARM     (* 0x5 *)
| ARM64   (* 0xc *)
| IA64    (* 0x6 *)
| Intel   (* 0x0 *)
| Unknown (* 0xffff *)


(** Predefined version information strings (see VerQueryValueW) *)
type win32_non_fixed_version_info = {
  comments: string option;
  companyName: string option;
  fileDescription: string option;
  fileVersionString: string option;
  internalName: string option;
  legalCopyright: string option;
  legalTrademarks: string option;
  originalFilename: string option;
  productName: string option;
  productVersionString: string option;
  privateBuild: string option;
  specialBuild: string option;
}

(** VS_FIXEDFILEINFO *)
type win32_version_info = {
  signature: int; (** [0xFEEF04BD] *)
  version: int * int; (** Structure version number *)
  fileVersion: int * int * int * int; (** File version *)
  productVersion: int * int * int * int; (** Product version *)
  fileFlagsMask: int; (** Valid bits in {!fileFlags} *)
  fileFlags: int; (** File attributes (see VS_FIXEDFILEINFO) *)
  fileOS: int; (** File OS (see VS_FIXEDFILEINFO) *)
  fileType: int; (** File Type (see VS_FIXEDFILEINFO) *)
  fileSubtype: int; (** File Sub-type (see VS_FIXEDFILEINFO) *)
  fileDate: int64; (** File creation time stamp *)
  strings: ((int * int) * win32_non_fixed_version_info) list;
    (** Non-fixed string table. First field is a pair of Language and Codepage ID. *)
}

external is_executable : string -> bool = "opam_is_executable"
(** faccessat on Unix; _waccess on Windows. Checks whether a path is executable
    for the current process. On Unix, unlike Unix.access, this is checked using
    the EUID/EGID rather than RUID/RGID. *)
