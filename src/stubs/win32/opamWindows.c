/**************************************************************************/
/*                                                                        */
/*    Copyright 2015, 2016, 2017, 2018 MetaStack Solutions Ltd.           */
/*                                                                        */
/*  All rights reserved. This file is distributed under the terms of the  */
/*  GNU Lesser General Public License version 2.1, with the special       */
/*  exception on linking described in the file LICENSE.                   */
/*                                                                        */
/**************************************************************************/

/* We need the UTF16 conversion functions (which prior to 4.13 were internal)
   and also the internal length calculations functions, which are still
   internal. */
#define CAML_INTERNALS

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/version.h>
#include <caml/osdeps.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>

#include <Windows.h>
#include <Shlobj.h>
#include <TlHelp32.h>

#include <stdio.h>

static struct custom_operations HandleOps =
{
  "org.ocaml.opam.Win32.Handle/1",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

#define HANDLE_val(v) (*((HANDLE*)Data_custom_val(v)))

typedef BOOL (WINAPI *LPFN_ISWOW64PROCESS2) (HANDLE, USHORT *, USHORT *);

static LPFN_ISWOW64PROCESS2 pIsWow64Process2 = NULL;

/*
 * Taken from otherlibs/unix/winwait.c (sadly declared static)
 */
static value alloc_process_status(HANDLE pid, int status)
{
  CAMLparam0();
  CAMLlocal1(st);
  value res;

  st = caml_alloc_small(1, 0);
  Field(st, 0) = Val_int(status);
  res = caml_alloc_small(2, 0);
  Field(res, 0) = Val_long((intnat) pid);
  Field(res, 1) = st;
  CAMLreturn(res);
}

/* Order must match OpamStubsTypes.registry_root */
static HKEY roots[] =
  {HKEY_CLASSES_ROOT,
   HKEY_CURRENT_CONFIG,
   HKEY_CURRENT_USER,
   HKEY_LOCAL_MACHINE,
   HKEY_USERS};

char* InjectSetEnvironmentVariable(DWORD, LPCWSTR, LPCWSTR);

/* Actual primitives from here */
CAMLprim value OPAMW_GetCurrentProcessID(value unit)
{
  return caml_copy_int32(GetCurrentProcessId());
}

CAMLprim value OPAMW_GetStdHandle(value nStdHandle)
{
  value result;

  HANDLE hResult;

  if ((hResult = GetStdHandle(-Int_val(nStdHandle) - 10)) == NULL)
    caml_raise_not_found();

  result = caml_alloc_custom(&HandleOps, sizeof(HANDLE), 0, 1);
  HANDLE_val(result) = hResult;

  return result;
}

CAMLprim value OPAMW_GetConsoleScreenBufferInfo(value hConsoleOutput)
{
  CAMLparam0();
  CAMLlocal1(result);
  value coord;
  CONSOLE_SCREEN_BUFFER_INFO buffer;

  if (!GetConsoleScreenBufferInfo(HANDLE_val(hConsoleOutput), &buffer))
    caml_raise_not_found();

  result = caml_alloc(5, 0);
  coord = caml_alloc_small(2, 0);
  Field(coord, 0) = Val_int(buffer.dwSize.X);
  Field(coord, 1) = Val_int(buffer.dwSize.Y);
  Store_field(result, 0, coord);
  coord = caml_alloc_small(2, 0);
  Field(coord, 0) = Val_int(buffer.dwCursorPosition.X);
  Field(coord, 1) = Val_int(buffer.dwCursorPosition.Y);
  Store_field(result, 1, coord);
  Store_field(result, 2, Val_int(buffer.wAttributes));
  coord = caml_alloc_small(4, 0);
  Field(coord, 0) = Val_int(buffer.srWindow.Left);
  Field(coord, 1) = Val_int(buffer.srWindow.Top);
  Field(coord, 2) = Val_int(buffer.srWindow.Right);
  Field(coord, 3) = Val_int(buffer.srWindow.Bottom);
  Store_field(result, 3, coord);
  coord = caml_alloc_small(2, 0);
  Field(coord, 0) = Val_int(buffer.dwMaximumWindowSize.X);
  Field(coord, 1) = Val_int(buffer.dwMaximumWindowSize.Y);
  Store_field(result, 4, coord);

  CAMLreturn(result);
}

CAMLprim value OPAMW_SetConsoleTextAttribute(value hConsoleOutput,
                                             value wAttributes)
{
  if (!SetConsoleTextAttribute(HANDLE_val(hConsoleOutput),
                               Int_val(wAttributes)))
    caml_failwith("setConsoleTextAttribute");

  return Val_unit;
}

CAMLprim value OPAMW_FillConsoleOutputCharacter(value vhConsoleOutput,
                                                value character,
                                                value vnLength,
                                                value vdwWriteCoord)
{
  HANDLE hConsoleOutput = HANDLE_val(vhConsoleOutput);
  CONSOLE_SCREEN_BUFFER_INFO ConsoleScreenBufferInfo;
  WCHAR cCharacter = Int_val(character) & 0xFF;
  DWORD nLength = Int_val(vnLength);
  COORD dwWriteCoord =
    {Int_val(Field(vdwWriteCoord, 0)), Int_val(Field(vdwWriteCoord, 1))};
  DWORD dwNumberOfCharsWritten;
  BOOL result = FALSE;

  if (GetConsoleScreenBufferInfo(hConsoleOutput, &ConsoleScreenBufferInfo))
  {
    while ((result = FillConsoleOutputCharacter(hConsoleOutput,
                                                cCharacter,
                                                nLength,
                                                dwWriteCoord,
                                                &dwNumberOfCharsWritten))
           && dwNumberOfCharsWritten != nLength)
    {
      nLength -= dwNumberOfCharsWritten;
      dwWriteCoord.X += dwNumberOfCharsWritten;
      dwWriteCoord.Y += dwWriteCoord.X / ConsoleScreenBufferInfo.dwSize.X;
      dwWriteCoord.X %= ConsoleScreenBufferInfo.dwSize.X;
    }
  }

  return Val_bool(result);
}

CAMLprim value OPAMW_GetConsoleMode(value hConsoleHandle)
{
  DWORD dwMode;
  if (!GetConsoleMode(HANDLE_val(hConsoleHandle), &dwMode))
    caml_raise_not_found();

  return Val_int(dwMode);
}

CAMLprim value OPAMW_SetConsoleMode(value hConsoleMode, value dwMode)
{
  BOOL result = SetConsoleMode(HANDLE_val(hConsoleMode), Int_val(dwMode));

  return Val_bool(result);
}

CAMLprim value OPAMW_GetWindowsVersion(value unit)
{
  value result;
  result = caml_alloc_small(4, 0);
  Field(result, 0) = Val_int(caml_win32_major);
  Field(result, 1) = Val_int(caml_win32_minor);
  Field(result, 2) = Val_int(caml_win32_build);
  Field(result, 3) = Val_int(caml_win32_revision);

  return result;
}

static inline value get_native_cpu_architecture(void)
{
  SYSTEM_INFO SystemInfo;
  GetNativeSystemInfo(&SystemInfo);
  switch (SystemInfo.wProcessorArchitecture) {
  case PROCESSOR_ARCHITECTURE_AMD64:
    return Val_int(0);
  case PROCESSOR_ARCHITECTURE_ARM:
    return Val_int(1);
  case PROCESSOR_ARCHITECTURE_ARM64:
    return Val_int(2);
  case PROCESSOR_ARCHITECTURE_IA64:
    return Val_int(3); /* Wow! */
  case PROCESSOR_ARCHITECTURE_INTEL:
    return Val_int(4);
  default: /* PROCESSOR_ARCHITECTURE_UNKNOWN */
    return Val_int(5);
  }
}

static inline value get_PE_cpu_architecture(USHORT image)
{
  switch (image) {
  case IMAGE_FILE_MACHINE_I386:
    return Val_int(4);
  case IMAGE_FILE_MACHINE_ARM:
  case IMAGE_FILE_MACHINE_THUMB:
  case IMAGE_FILE_MACHINE_ARMNT:
    return Val_int(1);
  case IMAGE_FILE_MACHINE_IA64:
    return Val_int(3); /* Wow! */
  case IMAGE_FILE_MACHINE_AMD64:
    return Val_int(0);
  case IMAGE_FILE_MACHINE_ARM64:
    return Val_int(2);
  default:
    return Val_int(5);
  }
}

CAMLprim value OPAMW_GetArchitecture(value unit)
{
  USHORT ProcessMachine, NativeMachine;

  if (!pIsWow64Process2)
    pIsWow64Process2 =
      (LPFN_ISWOW64PROCESS2)GetProcAddress(GetModuleHandle(L"kernel32"), "IsWow64Process2");

  if (!pIsWow64Process2 || !pIsWow64Process2(GetCurrentProcess(), &ProcessMachine, &NativeMachine))
    return get_native_cpu_architecture();
  else
    return get_PE_cpu_architecture(NativeMachine);
}

CAMLprim value OPAMW_GetProcessArchitecture(value pid)
{
  HANDLE hProcess;
  USHORT ProcessMachine, NativeMachine;
  BOOL Wow64Process;
  value result;

  if (!pIsWow64Process2)
    pIsWow64Process2 =
      (LPFN_ISWOW64PROCESS2)GetProcAddress(GetModuleHandle(L"kernel32"), "IsWow64Process");

  if (Is_long(pid)) {
    hProcess = GetCurrentProcess();
  } else {
    hProcess =
      OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, FALSE, Int32_val(Field(pid, 0)));
  }

  if (!pIsWow64Process2 || !pIsWow64Process2(hProcess, &ProcessMachine, &NativeMachine))
    if (IsWow64Process(hProcess, &Wow64Process) && Wow64Process)
      /* Must be x86_32 */
      result = Val_int(4);
    else
      /* Will be either x86_32 or x86_64 */
      result = get_native_cpu_architecture();
  else
    if (ProcessMachine == IMAGE_FILE_MACHINE_UNKNOWN)
      result = get_PE_cpu_architecture(NativeMachine);
    else
      result = get_PE_cpu_architecture(ProcessMachine);

  if (Is_block(pid))
    CloseHandle(hProcess);

  return result;
}

/*
 * Adapted from otherlibs/win32unix/winwait.c win_waitpid
 */
CAMLprim value OPAMW_waitpids(value vpid_reqs, value vpid_len)
{
  int i;
  DWORD status, retcode;
  HANDLE pid_req;
  DWORD err = 0;
  int len = Int_val(vpid_len);
  value ptr = vpid_reqs;
  HANDLE *lpHandles = (HANDLE*)malloc(sizeof(HANDLE) * len);

  if (lpHandles == NULL)
    caml_raise_out_of_memory();

  for (i = 0; i < len; i++) {
    lpHandles[i] = (HANDLE)Long_val(Field(ptr, 0));
    ptr = Field(ptr, 1);
  }

  caml_enter_blocking_section();
  retcode = WaitForMultipleObjects(len, lpHandles, FALSE, INFINITE);
  if (retcode == WAIT_FAILED) err = GetLastError();
  caml_leave_blocking_section();
  if (err) {
    win32_maperr(err);
    uerror("waitpids", Nothing);
  }
  pid_req = lpHandles[retcode - WAIT_OBJECT_0];
  free(lpHandles);
  if (! GetExitCodeProcess(pid_req, &status)) {
    win32_maperr(GetLastError());
    uerror("waitpids", Nothing);
  }

  /*
   * NB Unlike in win_waitpid, it's not possible to have status == STILL_ACTIVE
   */
  CloseHandle(pid_req);
  return alloc_process_status(pid_req, status);
}

CAMLprim value OPAMW_WriteRegistry(value hKey,
                                   value sub_key,
                                   value value_name,
                                   value dwType,
                                   value data)
{
  HKEY key;
  LPVOID lpData = NULL;
  DWORD cbData = 0;
  DWORD type = 0;
  LSTATUS ret;
  LPWSTR lpSubKey, lpValueName;

  if (!caml_string_is_c_safe(sub_key) || !caml_string_is_c_safe(value_name))
    caml_invalid_argument("OPAMW_WriteRegistry");

  /* Cases match OpamStubsTypes.registry_value */
  switch (Int_val(dwType))
  {
    case 0:
      lpData = caml_stat_strdup_to_utf16(String_val(data));
      cbData = win_multi_byte_to_wide_char(String_val(data), -1, NULL, 0);
      type = REG_SZ;
      break;
    default:
      caml_failwith("OPAMW_WriteRegistry: value not implemented");
      break;
  }

  if (!(lpSubKey = caml_stat_strdup_to_utf16(String_val(sub_key)))) {
    caml_stat_free(lpData);
    caml_raise_out_of_memory();
  }
  if (!(lpValueName = caml_stat_strdup_to_utf16(String_val(value_name)))) {
    caml_stat_free(lpData);
    caml_stat_free(lpSubKey);
    caml_raise_out_of_memory();
  }

  ret =
    RegSetKeyValue(roots[Int_val(hKey)], lpSubKey, lpValueName, type, lpData, cbData);

  caml_stat_free(lpSubKey);
  caml_stat_free(lpValueName);
  caml_stat_free(lpData);

  if (ret == ERROR_FILE_NOT_FOUND)
    caml_raise_not_found();
  else if (ret != ERROR_SUCCESS)
    caml_failwith("RegSetKeyValue");

  return Val_unit;
}

CAMLprim value OPAMW_GetConsoleOutputCP(value unit)
{
  return Val_int(GetConsoleOutputCP());
}

CAMLprim value OPAMW_GetCurrentConsoleFontEx(value hConsoleOutput,
                                             value bMaximumWindow)
{
  CAMLparam0();
  CAMLlocal1(result);
  value coord;
  int len;
  CONSOLE_FONT_INFOEX fontInfo;
  fontInfo.cbSize = sizeof(fontInfo);

  if (GetCurrentConsoleFontEx(HANDLE_val(hConsoleOutput),
                              Bool_val(bMaximumWindow),
                              &fontInfo))
  {
    result = caml_alloc_tuple(5);
    Store_field(result, 0, Val_int(fontInfo.nFont));
    coord = caml_alloc_small(2, 0);
    Field(coord, 0) = Val_int(fontInfo.dwFontSize.X);
    Field(coord, 1) = Val_int(fontInfo.dwFontSize.Y);
    Store_field(result, 1, coord);
    Store_field(result, 2, Val_int(fontInfo.FontFamily));
    Store_field(result, 3, Val_int(fontInfo.FontWeight));
    Store_field(result, 4, caml_copy_string_of_utf16(fontInfo.FaceName));
  }
  else
  {
    caml_raise_not_found();
  }

  CAMLreturn(result);
}

CAMLprim value OPAMW_CreateGlyphChecker(value fontName)
{
  CAMLparam0();
  CAMLlocal1(result);
  value handle;
  HDC hDC;

  if (!caml_string_is_c_safe(fontName))
    caml_invalid_argument("OPAMW_CreateGlyphChecker");

  /*
   * Any device context will do to load the font, so use the Screen DC.
   */
  hDC = GetDC(NULL);

  if (hDC)
  {
    wchar_t* lpszFace = caml_stat_strdup_to_utf16(String_val(fontName));
    HFONT hFont =
      CreateFontW(0, 0, 0, 0, FW_DONTCARE, FALSE, FALSE, FALSE, DEFAULT_CHARSET,
                  OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY,
                  DEFAULT_PITCH, lpszFace);
    caml_stat_free(lpszFace);

    if (hFont)
    {
      if (SelectObject(hDC, hFont))
      {
        result = caml_alloc_tuple(2);
        handle = caml_alloc_custom(&HandleOps, sizeof(HANDLE), 0, 1);
        HANDLE_val(handle) = hDC;
        Store_field(result, 0, handle);
        handle = caml_alloc_custom(&HandleOps, sizeof(HANDLE), 0, 1);
        HANDLE_val(handle) = hFont;
        Store_field(result, 1, handle);
      }
      else
      {
        caml_failwith("OPAMW_CheckGlyphs: SelectObject");
      }
    }
    else
    {
      caml_failwith("OPAMW_CheckGlyphs: CreateFontW");
    }
  }
  else
  {
    caml_failwith("OPAMW_CheckGlyphs: GetDC");
  }

  CAMLreturn(result);
}

CAMLprim value OPAMW_DeleteGlyphChecker(value checker)
{
  DeleteObject(HANDLE_val(Field(checker, 1)));
  ReleaseDC(NULL, HANDLE_val(Field(checker, 0)));

  return Val_unit;
}

CAMLprim value OPAMW_HasGlyph(value checker, value scalar)
{
  BOOL result = FALSE;
  HDC hDC = HANDLE_val(Field(checker, 0));

  WCHAR test = (WCHAR)Int_val(scalar);
  WORD index = 0;

  switch (GetGlyphIndicesW(hDC, &test, 1, &index, GGI_MARK_NONEXISTING_GLYPHS))
  {
    case 1:
      break;
    case GDI_ERROR:
      caml_failwith("OPAMW_CheckGlyphs: GetGlyphIndicesW");
    default:
      caml_failwith("OPAMW_CheckGlyphs: GetGlyphIndicesW (unexpected return)");
  }

  return Val_bool(index != 0xffff);
}

/*
 * OPAMW_process_putenv is implemented using Process Injection.
 * Idea inspired by Bill Stewart's editvar
 *   (see http://www.westmesatech.com/editv.html)
 * Full technical details at http://www.codeproject.com/Articles/4610/Three-Ways-to-Inject-Your-Code-into-Another-Proces#section_3
 */

CAMLprim value OPAMW_process_putenv(value pid, value key, value val)
{
  char* result;
  LPWSTR lpName, lpValue;
  DWORD dwProcessId = Int32_val(pid);

  if (!caml_string_is_c_safe(key) || !caml_string_is_c_safe(val))
    caml_invalid_argument("OPAMW_process_putenv");

  /*
   * MSDN is all over the place as to what the technical limits are for
   * environment variables (looks like 32KiB for both both name and value)
   * however there's no need to inject 64KiB data each time - hence 4KiB limit.
   */
  if (caml_string_length(key) > 4095 || caml_string_length(val) > 4095)
    caml_invalid_argument("Strings too long");

  if (!(lpName = caml_stat_strdup_to_utf16(String_val(key))))
    caml_raise_out_of_memory();
  if (!(lpValue = caml_stat_strdup_to_utf16(String_val(val)))) {
    caml_stat_free(lpName);
    caml_raise_out_of_memory();
  }

  caml_enter_blocking_section();
  result = InjectSetEnvironmentVariable(dwProcessId, lpName, lpValue);
  caml_leave_blocking_section();

  if (result == NULL)
    return Val_true;
  else if (strlen(result) == 0)
    return Val_false;
  else
    caml_failwith(result);
}

/*
 * Somewhat against my better judgement, wrap SHGetFolderPath rather than
 * SHGetKnownFolderPath to maintain XP compatibility. OPAM already requires
 * Windows Vista+ because of GetCurrentConsoleFontEx, but there may be a
 * workaround for that for XP lusers.
 */
CAMLprim value OPAMW_SHGetFolderPath(value nFolder, value dwFlags)
{
  WCHAR szPath[MAX_PATH];

  if (SUCCEEDED(SHGetFolderPath(NULL,
                                Int_val(nFolder),
                                NULL,
                                Int_val(dwFlags),
                                szPath)))
    return caml_copy_string_of_utf16(szPath);
  else
    caml_failwith("OPAMW_SHGetFolderPath");
}

CAMLprim value OPAMW_SendMessageTimeout(value vhWnd,
                                        value uTimeout,
                                        value fuFlags,
                                        value vmsg,
                                        value vwParam,
                                        value vlParam)
{
  value result;

  DWORD_PTR dwReturnValue;
  HRESULT lResult;
  WPARAM wParam;
  LPARAM lParam;
  UINT msg;
  HWND hWnd = (HWND)Nativeint_val(vhWnd);
  LPWSTR lParam_string = NULL;

  if (Int_val(vmsg) == 0) {
    msg = WM_SETTINGCHANGE;
    wParam = Int_val(vwParam);
    if (!caml_string_is_c_safe(vlParam))
      caml_invalid_argument("OPAMW_SendMessageTimeout");
    if (!(lParam_string = caml_stat_strdup_to_utf16(String_val(vlParam))))
      caml_raise_out_of_memory();
    lParam = (LPARAM)lParam_string;
  } else {
    caml_failwith("OPAMW_SendMessageTimeout: message not implemented");
  }

  caml_enter_blocking_section();
  lResult =
    SendMessageTimeout(hWnd, msg, wParam, lParam, Int_val(fuFlags),
                       Int_val(uTimeout), &dwReturnValue);
  caml_leave_blocking_section();

  if (lParam_string)
    caml_stat_free(lParam_string);

  result = caml_alloc_small(2, 0);
  Field(result, 0) = Val_int(lResult);
  Field(result, 1) = Val_int(dwReturnValue);
  return result;
}

CAMLprim value OPAMW_SendMessageTimeout_byte(value * v, int n)
{
  return OPAMW_SendMessageTimeout(v[0], v[1], v[2], v[3], v[4], v[5]);
}

CAMLprim value OPAMW_GetProcessAncestry(value unit)
{
  CAMLparam0();
  CAMLlocal3(result, tail, info);
  PROCESSENTRY32 entry;
  HANDLE hProcessSnapshot, hProcess;
  value cell;
  ULARGE_INTEGER *processes, *cur;
  int capacity = 512;
  int length = 0;
  DWORD target = GetCurrentProcessId();
  BOOL read_entry = TRUE;
  WCHAR ExeName[MAX_PATH + 1];
  DWORD dwSize;

  result = caml_alloc_small(2, 0);
  Field(result, 0) = Val_int(0);
  Field(result, 1) = Val_int(0);
  tail = result;

  /* Snapshot running processes */
  hProcessSnapshot = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if (hProcessSnapshot != INVALID_HANDLE_VALUE) {
    entry.dwSize = sizeof(PROCESSENTRY32);
    /* Read the first entry (just because it's a special function) */
    if (Process32First(hProcessSnapshot, &entry)) {
      if ((processes = (ULARGE_INTEGER *)malloc(capacity * sizeof(ULARGE_INTEGER)))) {
        /* Initialise the processes array */
        if (entry.th32ProcessID == 0) {
          processes->QuadPart = 0LL;
        } else {
          length = 1;
          processes->LowPart = entry.th32ProcessID;
          processes->HighPart = entry.th32ParentProcessID;
          processes[1].QuadPart = 0LL;
        }

        /* Build the process tree, starting with the current process */
        do {
          /* First search through processes we've already read */
          for (cur = processes; cur->QuadPart != 0; cur++) {
            if (cur->LowPart == target)
              break;
          }

          if (cur->QuadPart == 0LL) {
            /* Keep reading process entries until we reach the end of the list */
            while ((read_entry = Process32Next(hProcessSnapshot, &entry))) {
              if (entry.th32ProcessID != 0) {
                if (++length >= capacity) {
                  ULARGE_INTEGER *ptr;
                  capacity += 512;
                  ptr = (ULARGE_INTEGER *)realloc(processes, capacity * sizeof(ULARGE_INTEGER));
                  if (ptr == NULL) {
                    read_entry = FALSE;
                    break;
                  } else {
                    processes = ptr;
                  }
                }
                cur->LowPart = entry.th32ProcessID;
                cur->HighPart = entry.th32ParentProcessID;
                if (cur->LowPart == target) {
                  cur[1].QuadPart = 0LL;
                  break;
                } else {
                  cur++;
                }
              }
            }
            if (!read_entry)
              break;
          }

          /* Found it - construct the list entry */
          hProcess = OpenProcess(PROCESS_QUERY_LIMITED_INFORMATION, FALSE, target);
          if (hProcess != NULL) {
            dwSize = MAX_PATH + 1;
            if (!QueryFullProcessImageName(hProcess, 0, ExeName, &dwSize))
              ExeName[0] = L'\0';
            CloseHandle(hProcess);
          } else {
            ExeName[0] = L'\0';
          }
          info = caml_alloc_tuple(2);
          Store_field(info, 0, caml_copy_int32(target));
          Store_field(info, 1, caml_copy_string_of_utf16(ExeName));
          cell = caml_alloc_small(2, 0);
          Field(cell, 0) = info;
          Field(cell, 1) = Val_int(0);
          Store_field(tail, 1, cell);
          tail = cell;
          /* Search for this process's parent on the next round */
          target = cur->HighPart;
          /* Guard against looping by zeroing out the parent */
          cur->HighPart = 0;
        } while (1);
      }
      free(processes);
    }

    CloseHandle(hProcessSnapshot);
  }

  CAMLreturn(Field(result, 1));
}

CAMLprim value OPAMW_GetConsoleAlias(value alias, value exe_name)
{
  value result;

  DWORD nLength = 8192;
  LPWSTR lpSource, lpTargetBuffer, lpExeName;

  if (!caml_string_is_c_safe(alias) || !caml_string_is_c_safe(exe_name))
    caml_invalid_argument("OPAMW_GetConsoleAlias");

  if (!(lpTargetBuffer = (LPWSTR)malloc(nLength * sizeof(WCHAR))))
    caml_raise_out_of_memory();
  if (!(lpExeName = caml_stat_strdup_to_utf16(String_val(exe_name)))) {
    free(lpTargetBuffer);
    caml_raise_out_of_memory();
  }
  ;
  if (!(lpSource = caml_stat_strdup_to_utf16(String_val(alias)))) {
    free(lpTargetBuffer);
    caml_stat_free(lpExeName);
    caml_raise_out_of_memory();
  }

  if (GetConsoleAlias(lpSource, lpTargetBuffer, nLength, lpExeName))
    result = caml_copy_string_of_utf16(lpTargetBuffer);
  else
    result = caml_alloc_string(0);

  free(lpTargetBuffer);
  caml_stat_free(lpExeName);
  caml_stat_free(lpSource);

  return result;
}
