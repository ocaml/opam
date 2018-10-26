/**************************************************************************/
/*                                                                        */
/*    Copyright 2015, 2016, 2017, 2018 MetaStack Solutions Ltd.           */
/*                                                                        */
/*  OPAM is distributed in the hope that it will be useful, but WITHOUT   */
/*  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    */
/*  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public       */
/*  License for more details.                                             */
/*                                                                        */
/**************************************************************************/

#include <stdio.h>
/*
 * This will be being built for a different architecture, so it's easier just to
 * #include the code, rather than having to deal with .o(obj) files for
 * different architectures.
 */
#include "../stubs/opamInject.c"

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
int main(int argc, char *argv[], char *envp[])
{
  if (argc != 2)
  {
    printf("Invalid command line: this utility is an internal part of OPAM\n");
  }
  else
  {
    DWORD pid = atoi(argv[1]);
    BOOL running = TRUE;
    char* key = (char*)malloc(4097);
    char* value = (char*)malloc(4097);

    while (running)
    {
      if (fgets(key, 4097, stdin))
      {
        if (strcmp(key, "::QUIT\n") && fgets(value, 4097, stdin))
        {
          key[strlen(key) - 1] = value[strlen(value) - 1] = '\0';
          InjectSetEnvironmentVariable(pid, key, value);
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
    free(key);
    free(value);
  }
}
