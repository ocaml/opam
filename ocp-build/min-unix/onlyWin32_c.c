/******************************************************************************/
/*                                                                            */
/*                          TypeRex OCaml Tools                               */
/*                                                                            */
/*                               OCamlPro                                     */
/*                                                                            */
/*    Copyright 2011-2012 OCamlPro                                            */
/*    All rights reserved.  See accompanying files for the terms under        */
/*    which this file is distributed. In doubt, contact us at                 */
/*    contact@ocamlpro.com (http://www.ocamlpro.com/)                         */
/*                                                                            */
/******************************************************************************/

#ifdef ALSO__CYGWIN__
#define _WIN32
#endif

#ifdef _WIN32

#include <windows.h>
#include <sys/types.h>

#endif

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/signals.h>

#ifndef CAML_UNIXSUPPORT_H
#include <caml/unixsupport.h>
#define CAML_UNIXSUPPORT_H
#endif

#ifdef _WIN32
static value alloc_process_status(HANDLE pid, int status)
{
  value res, st;

  st = alloc(1, 0);
  Field(st, 0) = Val_int(status);
  Begin_root (st);
    res = alloc_small(2, 0);
    Field(res, 0) = Val_long((intnat) pid);
    Field(res, 1) = st;
  End_roots();
  return res;
}

enum { CAML_WNOHANG = 1, CAML_WUNTRACED = 2 };

static int wait_flag_table[] = { CAML_WNOHANG, CAML_WUNTRACED };

CAMLprim value onlyWin32_waitpids_ml(value ncount_v, value pid_reqs_v)
{
  int flags,i;
  DWORD status, retcode;
  DWORD err = 0;
  int ncount = Int_val(ncount_v);
  HANDLE* pid_reqs;
  HANDLE pid_req;

  pid_reqs = malloc(sizeof(HANDLE) * ncount);
  for(i=0; i < ncount; i++){
     pid_reqs[i] = (HANDLE) Long_val(Field(pid_reqs_v,i));
  }
  enter_blocking_section();
  retcode = WaitForMultipleObjects(ncount, pid_reqs, FALSE,INFINITE);
  if (retcode == WAIT_FAILED) err = GetLastError();
  leave_blocking_section();
  if (err) {
    free(pid_reqs);
    win32_maperr(err);
    uerror("waitpids", Nothing);
  }
  pid_req = pid_reqs[retcode - WAIT_OBJECT_0];
  free(pid_reqs);
  if (! GetExitCodeProcess(pid_req, &status)) {
    win32_maperr(GetLastError());
    uerror("waitpids", Nothing);
  }
  if (status == STILL_ACTIVE)
    return alloc_process_status((HANDLE) 0, 0);
  else {
    CloseHandle(pid_req);
    return alloc_process_status(pid_req, status);
  }
}

static int onlyWin32_has_console(void)
{
  HANDLE h, log;
  int i;

  h = CreateFile("CONOUT$", GENERIC_WRITE, FILE_SHARE_WRITE, NULL,
                 OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
  if (h == INVALID_HANDLE_VALUE) {
    return 0;
  } else {
    CloseHandle(h);
    return 1;
  }
}

value onlyWin32_create_process_chdir_native(value cmd, value cmdline, value env,
				  value fd1, value fd2, value fd3, value maybe_chdir)
{
  PROCESS_INFORMATION pi;
  STARTUPINFO si;
  char * exefile, * envp;
  int flags;
  LPCTSTR lpCurrentDirectory = NULL;

  exefile = search_exe_in_path(String_val(cmd));
  if (env != Val_int(0)) {
    envp = String_val(Field(env, 0));
  } else {
    envp = NULL;
  }
  /* Prepare stdin/stdout/stderr redirection */
  ZeroMemory(&si, sizeof(STARTUPINFO));
  si.cb = sizeof(STARTUPINFO);
  si.dwFlags = STARTF_USESTDHANDLES;
  si.hStdInput = Handle_val(fd1);
  si.hStdOutput = Handle_val(fd2);
  si.hStdError = Handle_val(fd3);
  /* If we do not have a console window, then we must create one
     before running the process (keep it hidden for apparence).
     If we are starting a GUI application, the newly created
     console should not matter. */
  if (win32_has_console())
    flags = 0;
  else {
    flags = CREATE_NEW_CONSOLE;
    si.dwFlags = (STARTF_USESHOWWINDOW | STARTF_USESTDHANDLES);
    si.wShowWindow = SW_HIDE;
  }
  if( maybe_chdir != Val_int(0) ){
    lpCurrentDirectory = String_val(Field(maybe_chdir,0));
  }
  /* Create the process */
  if (! CreateProcess(exefile, String_val(cmdline), NULL, NULL,
                      TRUE, flags, envp, lpCurrentDirectory, &si, &pi)) {
    win32_maperr(GetLastError());
    uerror("create_process", cmd);
  }
  CloseHandle(pi.hThread);
  /* Return the process handle as pseudo-PID
     (this is consistent with the wait() emulation in the MSVC C library */
  return Val_long(pi.hProcess);
}

#else

value onlyWin32_waitpids_ml(value ncount_v, value pid_reqs_v){
   uerror("onlyWin32_waitpids_ml", Nothing);
}

value onlyWin32_create_process_chdir_native(value cmd, value cmdline, value env,
				  value fd1, value fd2, value fd3, value maybe_chdir)
{
   uerror("onlyWin32_create_process_native", Nothing);
}

#endif

CAMLprim value onlyWin32_create_process_chdir(value * argv, int argn)
{
  return onlyWin32_create_process_chdir_native(argv[0], argv[1], argv[2],
				     argv[3], argv[4], argv[5], argv[6]);
}


#ifdef _WIN32

DOUBLE FileTime_to_POSIX(FILETIME ft)
{
  ULARGE_INTEGER date, adjust;
  date.HighPart = ft.dwHighDateTime;
  date.LowPart = ft.dwLowDateTime;

  // 100-nanoseconds = milliseconds * 10000
  adjust.QuadPart = 11644473600000ULL * 10000;
  // removes the diff between 1970 and 1601
  date.QuadPart -= adjust.QuadPart;
  // converts back from 100-nanoseconds to seconds
  return date.QuadPart / 1e7;
}


value onlyWin32_getFileInformationByHandle_ml(value handle_v)
{
  HANDLE handle = (HANDLE)handle_v;
  BY_HANDLE_FILE_INFORMATION fileInfo;
  CAMLparam0 ();
  CAMLlocal1 (v);
  ULARGE_INTEGER size, index;

  if( !GetFileInformationByHandle(handle, &fileInfo) ){
    DWORD err = GetLastError();
    win32_maperr(err);
    uerror("GetFileInformationByHandle", Nothing);
  }

  size.HighPart = fileInfo.nFileSizeHigh;
  size.LowPart = fileInfo.nFileSizeLow;
  index.HighPart = fileInfo.nFileIndexHigh;
  index.LowPart = fileInfo.nFileIndexLow;

  v = caml_alloc (8, 0);
  Store_field(v,0, Val_int(fileInfo.dwFileAttributes));
  Store_field(v, 1,
              caml_copy_double(FileTime_to_POSIX(fileInfo.ftCreationTime)));
  Store_field(v, 2,
              caml_copy_double(FileTime_to_POSIX(fileInfo.ftLastAccessTime)));
  Store_field(v, 3,
              caml_copy_double(FileTime_to_POSIX(fileInfo.ftLastWriteTime)));
  Store_field(v, 4, Val_int(fileInfo.dwVolumeSerialNumber));
  Store_field(v, 5, caml_copy_int64(size.QuadPart));
  Store_field(v, 6, Val_int(fileInfo.nNumberOfLinks));
  Store_field(v, 7, caml_copy_int64(index.QuadPart));

  CAMLreturn (v);
}

value onlyWin32_getFileInformationByName_ml(value filename_v)
{
  HANDLE hfile = CreateFile(String_val(filename_v), 0, 
			    FILE_SHARE_READ|FILE_SHARE_WRITE|FILE_SHARE_DELETE, 
			    NULL,   OPEN_EXISTING, 
			    FILE_FLAG_BACKUP_SEMANTICS, NULL);
  value res;

  if( hfile == INVALID_HANDLE_VALUE ){
    DWORD err = GetLastError();
    win32_maperr(err);
    uerror("GetFileInformationByName", Nothing);
  }
  res = onlyWin32_getFileInformationByHandle_ml((value)hfile);
  CloseHandle(hfile);
  return res;
}

#else

value onlyWin32_getFileInformationByHandle_ml(value handle_v)
{
  uerror("onlyWin32_getFileInformationByHandle_ml", Nothing);
}

value onlyWin32_getFileInformationByName_ml(value filename_v)
{
  uerror("onlyWin32_getFileInformationByName_ml", Nothing);
}
#endif

