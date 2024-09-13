/**************************************************************************/
/*                                                                        */
/*    Copyright 2015, 2016, 2017, 2018 MetaStack Solutions Ltd.           */
/*                                                                        */
/*  All rights reserved. This file is distributed under the terms of the  */
/*  GNU Lesser General Public License version 2.1, with the special       */
/*  exception on linking described in the file LICENSE.                   */
/*                                                                        */
/**************************************************************************/

#include <Windows.h>

/* SetEnvironmentVariable function pointer type */
typedef LRESULT (WINAPI *SETENVIRONMENTVARIABLE)(LPCWSTR,LPCWSTR);

/*
 * Data structure to pass to the remote thread
 */
typedef struct {
  SETENVIRONMENTVARIABLE SetEnvironmentVariable;
  WCHAR lpName[4096];
  WCHAR lpValue[4096];
  BOOL result;
} INJDATA, *PINJDATA;

/*
 * Code to inject into the parent process
 */
static DWORD WINAPI ThreadFunc (INJDATA *pData)
{
  /*
   * Call the provided function pointer with its two arguments and return the
   * result.
   */
  pData->result = pData->SetEnvironmentVariable(pData->lpName, pData->lpValue);

  return 0;
}

/*
 * This is a dummy function used to calculate the code size of ThreadFunc.
 * This assumes that the linker does not re-order the functions.
 *   If it's a worry, could make the symbols public and use /ORDER
 *     (http://msdn.microsoft.com/en-us/library/00kh39zz.aspx)
 *   Presumably there's a gcc equivalent for mingw.
 */
static void AfterThreadFunc (void)
{
  return;
}

char* InjectSetEnvironmentVariable(DWORD dwProcessId, LPCWSTR lpName, LPCWSTR lpValue)
{
  /*
   * Open the parent process for code injection
   */
  DWORD dwDesiredAccess = PROCESS_CREATE_THREAD | PROCESS_QUERY_INFORMATION |
                          PROCESS_VM_OPERATION | PROCESS_VM_WRITE | PROCESS_VM_READ;
  HANDLE hProcess = OpenProcess(dwDesiredAccess, FALSE, dwProcessId);
  INJDATA payload = {NULL, L"", L"", FALSE};
  INJDATA* pData;
  DWORD* pCode;
  const int codeSize = ((LPBYTE)AfterThreadFunc - (LPBYTE)ThreadFunc);
  HANDLE hThread;

  if (!hProcess)
    return "OPAMW_process_putenv: could not open parent process";

  payload.SetEnvironmentVariable =
    (SETENVIRONMENTVARIABLE)GetProcAddress(GetModuleHandle(L"kernel32"), "SetEnvironmentVariableW");

  /*
   * Set-up the instruction
   */
  wcscpy(payload.lpName, lpName);
  wcscpy(payload.lpValue, lpValue);

  /*
   * Allocate a page in the parent process to hold the instruction and copy the
   * payload to it.
   */
  pData =
    (INJDATA*)VirtualAllocEx(hProcess,
                             0,
                             sizeof(INJDATA),
                             MEM_COMMIT,
                             PAGE_READWRITE);
  if (!pData)
  {
    CloseHandle(hProcess);
    return "OPAMW_process_putenv: VirtualAllocEx (data) in parent failed";
  }
  if (!WriteProcessMemory(hProcess, pData, &payload, sizeof(INJDATA), NULL))
  {
    VirtualFreeEx(hProcess, pData, 0, MEM_RELEASE);
    CloseHandle(hProcess);
    return "OPAMW_process_putenv: could not copy data to parent process";
  }

  /*
   * Allocate a page in the parent process to hold ThreadFunc and copy the code
   * there.
   */
  pCode =
    (PDWORD)VirtualAllocEx(hProcess,
                           0,
                           codeSize,
                           MEM_COMMIT,
                           PAGE_EXECUTE_READWRITE);
  if (!pCode)
  {
    VirtualFreeEx(hProcess, pData, 0, MEM_RELEASE);
    CloseHandle(hProcess);
    return "OPAMW_process_putenv: VirtualAllocEx (exec) in parent failed";
  }
  if (!WriteProcessMemory(hProcess, pCode, &ThreadFunc, codeSize, NULL))
  {
    VirtualFreeEx(hProcess, pCode, 0, MEM_RELEASE);
    VirtualFreeEx(hProcess, pData, 0, MEM_RELEASE);
    CloseHandle(hProcess);
    return "OPAMW_process_putenv: could not copy code to parent process";
  }

  /*
   * Start the remote thread
   */
  hThread =
    CreateRemoteThread(hProcess,
                       NULL,
                       0,
                       (LPTHREAD_START_ROUTINE)pCode,
                       pData,
                       0,
                       NULL);
  if (!hThread)
  {
    VirtualFreeEx(hProcess, pCode, 0, MEM_RELEASE);
    VirtualFreeEx(hProcess, pData, 0, MEM_RELEASE);
    CloseHandle(hProcess);
    return "OPAMW_process_putenv: could not start remote thread in parent";
  }

  /*
   * Wait for the thread to terminate.
   */
  WaitForSingleObject(hThread, INFINITE);
  CloseHandle(hThread);

  /*
   * Get the result back
   */
  ReadProcessMemory(hProcess, pData, &payload, sizeof(INJDATA), NULL);

  /*
   * Release the memory
   */
  VirtualFreeEx(hProcess, pCode, 0, MEM_RELEASE);
  VirtualFreeEx(hProcess, pData, 0, MEM_RELEASE);
  CloseHandle(hProcess);

  return (payload.result ? NULL : "");
}
