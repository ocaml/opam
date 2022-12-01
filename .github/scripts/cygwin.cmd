@rem ***********************************************************************
@rem *                                                                     *
@rem *     Copyright 2021-2022 David Allsopp Ltd.                          *
@rem *                                                                     *
@rem *  All rights reserved. This file is distributed under the terms of   *
@rem *  the GNU Lesser General Public License version 2.1, with the        *
@rem *  special exception on linking described in the file LICENSE.        *
@rem *                                                                     *
@rem ***********************************************************************
@setlocal
@echo off

:: This script configures Cygwin32 or Cygwin64 either from a cached copy or by
:: downloading the Cygwin Setup program.
::
:: cygwin.cmd distro cache-directory {create|host}
::
:: where distro is x86_64-pc-cygwin and rebuilds the cache
::
:: Environment variables:
::   CYGWIN_ROOT        - Cygwin installation root directory
::   CYGWIN_MIRROR      - Package repository mirror

set CYGWIN_CACHE_DIR=%2
set CYGWIN_DISTRO=%1
if "%CYGWIN_DISTRO%" neq "x86_64-pc-cygwin" (
  echo Invalid Cygwin distro: %1
  exit /b 2
)

if "%3" equ "create" goto SetupCygwin

if not exist %CYGWIN_CACHE_DIR%\%CYGWIN_DISTRO%\cache.tar (
  echo Cache download failed - job failed
  exit /b 2
)

:: PATH is only set when we know we're restoring the cache because of an
:: inconsistency in actions/cache. If the cacher detects GNU tar, it will create
:: an archive compressed with zstd, but it doesn't realise that it will be unable
:: to restore that archive.
::
:: It's possible to dance around this issue by enabling the MSYS2 installation,
:: but that adds instability at a saving of only a few tens of megabytes on the
:: cache, so instead we require that the cache download is required and fail the
:: job otherwise.
:: COMBAK At present we clobber the PATH on purpose - this wants to be filtered or something
::        This should be filtered. The main thing is that %CYGWIN_ROOT%\bin is _before_
::        C:\Windows\system32 (overriding curl, bash, etc. in System32) but after Mercurial
::        and Git (so that they are not overridden).
set Path=C:\Program Files\Mercurial;C:\Program Files\Git\cmd;%CYGWIN_ROOT%\bin;C:\Windows\system32;C:\Windows;C:\Windows\System32\Wbem;C:\Windows\System32\WindowsPowerShell\v1.0\;C:\Windows\System32\OpenSSH\
if "%3" equ "i686-w64-mingw32" set Path=%CYGWIN_ROOT%\usr\%3\sys-root\mingw\bin;%Path%
if "%3" equ "x86_64-w64-mingw32" set Path=%CYGWIN_ROOT%\usr\%3\sys-root\mingw\bin;%Path%
if "%3" equ "x86_64-pc-cygwin" set Path=%CYGWIN_ROOT%\bin;%Path%

::echo %CYGWIN_ROOT%\bin>> %GITHUB_PATH%
echo Path=%Path%>> %GITHUB_ENV%

pushd %CYGWIN_CACHE_DIR%

if not exist %CYGWIN_ROOT%\bin\nul md %CYGWIN_ROOT%\bin
:: Restore tar.exe (%CYGWIN_ROOT%\bin is already in PATH)
copy %CYGWIN_DISTRO%\bootstrap\* %CYGWIN_ROOT%\bin\
:: Read the /cygdrive form of the installation root
for /f "delims=" %%P in (%CYGWIN_DISTRO%\restore) do set CYGWIN_ROOT_NATIVE=%%P
:: Bootstrap Cygwin
cygtar -pxf %CYGWIN_DISTRO%/cache.tar -C %CYGWIN_ROOT_NATIVE%

:: The tarballs don't include the /usr/bin/tar and /usr/bin/git symlinks
call :MungeSymlinks

popd

goto :EOF

:SetupCygwin

echo ::group::Installing Cygwin

:: The caching job sets up both Cygwin32 and Cygwin64, so ensure that any
:: previous installation is wiped.
if exist %CYGWIN_ROOT%\nul rd /s/q %CYGWIN_ROOT%
md %CYGWIN_ROOT%

:: Download the required setup program: the mingw-w64 compilers are only
:: installed with Cygwin64.
if "%1" equ "x86_64-pc-cygwin" (
  curl -sLo %CYGWIN_ROOT%\setup.exe https://cygwin.com/setup-x86_64.exe
  set CYGWIN_PACKAGES=,mingw64-i686-gcc-g++,mingw64-x86_64-gcc-g++
) else (
  curl -sLo %CYGWIN_ROOT%\setup.exe https://cygwin.com/setup-x86.exe
  set CYGWIN_PACKAGES=
)

:: libicu-devel is needed until an alternative to the uconv call in MungeSymlinks
:: is found
set CYGWIN_PACKAGES=make,patch,curl,diffutils,tar,unzip,git,gcc-g++,libicu-devel%CYGWIN_PACKAGES%

:: D:\cygwin-packages is specified just to keep the build directory clean; the
:: files aren't preserved.
%CYGWIN_ROOT%\setup.exe --quiet-mode --no-shortcuts --no-startmenu --no-desktop --only-site --root %CYGWIN_ROOT% --site "%CYGWIN_MIRROR%" --local-package-dir D:\cygwin-packages --packages %CYGWIN_PACKAGES%

:: This triggers the first-time copying of the skeleton files for the user.
:: The main reason for doing this is so that the noise on stdout doesn't mess
:: up the call to ldd later!
%CYGWIN_ROOT%\bin\bash -lc "uname -a"

:: Mitigations added for CVE-2022-24765 cause error messages as the owner of the
:: git checkout won't be correct. It simply doesn't matter on a CI system, so
:: just add the directory to safe.directory to ignore it.
%CYGWIN_ROOT%\bin\bash -c "git config --global --add safe.directory $PWD"

echo ::endgroup::

:: cygpath %CYGWIN_ROOT% will return / which isn't very helpful. Instead, call
:: cygpath on the drive letter (e.g. D: => /cygdrive/d) and then call cygpath
:: with that and the rest of the path (e.g. /cygdrive/d\cygwin => /cygdrive/d/cygwin)
for /f "delims=" %%P in ('%CYGWIN_ROOT%\bin\cygpath.exe %CYGWIN_ROOT:~0,2%') do set CYGWIN_ROOT_NATIVE=%%P
for /f "delims=" %%P in ('%CYGWIN_ROOT%\bin\cygpath.exe "%CYGWIN_ROOT_NATIVE%%CYGWIN_ROOT:~2%"') do set CYGWIN_ROOT_NATIVE=%%P
for /f "delims=" %%P in ('%CYGWIN_ROOT%\bin\cygpath.exe %CYGWIN_CACHE_DIR%') do set CYGWIN_CACHE_DIR_NATIVE=%%P

:: Now we have %CYGWIN_ROOT% in Windows format and %CYGWIN_ROOT_NATIVE% in
:: /cygdrive format and similarly for %CYGWIN_CACHE_DIR% and
:: %CYGWIN_CACHE_DIR_NATIVE%.

echo Cygwin installed in %CYGWIN_ROOT% ^(%CYGWIN_ROOT_NATIVE%^)
echo Cygwin cache maintained at %CYGWIN_CACHE_DIR% ^(%CYGWIN_CACHE_DIR_NATIVE%^)
 
:: Prevent tar and git being used from PATH outside Cygwin by renaming the binaries
%CYGWIN_ROOT%\bin\bash -lc "cd /usr/bin ; mv tar cygtar ; mv git cyggit"
call :MungeSymlinks

:: GitHub Actions uses Windows tar which is unable to process the LXSS symlinks
:: which Cygwin uses. So we use Cygwin's tar to zip up Cygwin and place its
:: tar.exe (along with the required DLLs) in %CYGWIN_CACHE_DIR%\bootstrap
if not exist %CYGWIN_CACHE_DIR%\%CYGWIN_DISTRO%\bootstrap\nul md %CYGWIN_CACHE_DIR%\%CYGWIN_DISTRO%\bootstrap

echo Setting up bootstrap process...
echo   - tar.exe
copy %CYGWIN_ROOT%\bin\cygtar.exe %CYGWIN_CACHE_DIR%\%CYGWIN_DISTRO%\bootstrap\ > nul
echo ./bin/cygtar.exe> D:\exclude
echo ./bin/tar>> D:\exclude
echo ./bin/git>> D:\exclude

:: Use Cygwin's ldd to determine the required DLLs for tar.exe
for /f "usebackq delims=" %%f in (`%CYGWIN_ROOT%\bin\bash -lc "ldd /bin/cygtar | sed -ne 's|.* => \(/usr/bin/.*\) ([^)]*)$|\1|p' | xargs cygpath -w"`) do (
  echo   - %%~nxf
  echo ./bin/%%~nxf>> D:\exclude
  copy %%f %CYGWIN_CACHE_DIR%\%CYGWIN_DISTRO%\bootstrap\ > nul
)

:: tar up the entire Cygwin installation, excluding the files we copied to
:: bootstrap. No compression since GitHub Actions caching will tar this again.
:: This operation has to be done from the /cygdrive form of the root, so that
:: the special files in /dev are correctly captured.
%CYGWIN_ROOT%\bin\bash -lc "tar -pcf %CYGWIN_CACHE_DIR_NATIVE%/%CYGWIN_DISTRO%/cache.tar --exclude-from=/cygdrive/d/exclude -C %CYGWIN_ROOT_NATIVE% ."

:: We won't have cygpath when restoring the archive, so write the path to
:: restore the cache to into the cache itself.
echo %CYGWIN_ROOT_NATIVE%> %CYGWIN_CACHE_DIR%\%CYGWIN_DISTRO%\restore

del D:\exclude

goto :EOF

:MungeSymlinks

%CYGWIN_ROOT%\bin\bash -lc "cd /usr/bin ; echo -n '!<symlink>' > tar ; echo -ne 'cygtar.exe\000' | uconv -t UTF16LE --add-signature >> tar ; echo -n '!<symlink>' > git ; echo -ne 'cyggit.exe\000' | uconv -t UTF16LE --add-signature >> git ; chattr -f +s git tar"

goto :EOF
