/**************************************************************************/
/*                                                                        */
/*    Copyright 2015, 2016, 2017, 2018 MetaStack Solutions Ltd.           */
/*                                                                        */
/*  All rights reserved. This file is distributed under the terms of the  */
/*  GNU Lesser General Public License version 2.1, with the special       */
/*  exception on linking described in the file LICENSE.                   */
/*                                                                        */
/**************************************************************************/

#include <stdio.h>
#include <fcntl.h>

/*
 * This will be being built for a different architecture, so it's easier just to
 * #include the code, rather than having to deal with .o(obj) files for
 * different architectures.
 */
#include "opamInject.c"

/*
 * This trivially simple utility takes a single PID and then reads CRLF
 * terminated lines from STDIN. The line ::QUIT causes the program to terminate
 * otherwise a further line is read and the two lines together form the
 * key/value pair to be set in the process's environment.
 *
 * This utility is always compiled x86 if OPAM is compiled x64 and vice versa
 * and allows OPAM to manipulate a parent whose architecture differs from its
 * own. When the architecture matches, OPAM injects the code itself, but
 * injecting from a 64-bit process to a 32-bit parent is quite hard (and
 * potentially unstable) and injecting from a 32-bit process to a 64-bit parent
 * is phenomenally hard!
 */
void loop(DWORD dwProcessId)
{
  BOOL running = TRUE;
  LPWSTR lpKey = (LPWSTR)malloc(4097 * sizeof(WCHAR));
  LPWSTR lpValue = (LPWSTR)malloc(4097 * sizeof(WCHAR));

  while (running)
  {
    if (fgetws(lpKey, 4097, stdin))
    {
      if (wcscmp(lpKey, L"::QUIT\n") && fgetws(lpValue, 4097, stdin))
      {
        lpKey[wcslen(lpKey) - 1] = lpValue[wcslen(lpValue) - 1] = L'\0';
        InjectSetEnvironmentVariable(dwProcessId, lpKey, lpValue);
      }
      else
      {
        running = FALSE;
      }
    }
    else
    {
      running = FALSE;
    }
  }
  free(lpKey);
  free(lpValue);
}

int wmain(int argc, wchar_t *argv[], wchar_t *envp[])
{
  _setmode(_fileno(stdin), _O_U16TEXT);
  if (argc != 2)
  {
    printf("Invalid command line: this utility is an internal part of OPAM\n");
    return 1;
  }

  loop(_wtoi(argv[1]));
  return 0;
}
