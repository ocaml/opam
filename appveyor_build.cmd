@rem ***********************************************************************
@rem *                                                                     *
@rem *                                 opam                                *
@rem *                                                                     *
@rem *                 David Allsopp, OCaml Labs, Cambridge.               *
@rem *                                                                     *
@rem *   Copyright 2018 MetaStack Solutions Ltd.                           *
@rem *                                                                     *
@rem *   All rights reserved.  This file is distributed under the terms of *
@rem *   the GNU Lesser General Public License version 2.1, with the       *
@rem *   special exception on linking described in the file LICENSE.       *
@rem *                                                                     *
@rem ***********************************************************************

@rem BE CAREFUL ALTERING THIS FILE TO ENSURE THAT ERRORS PROPAGATE
@rem IF A COMMAND SHOULD FAIL IT PROBABLY NEEDS TO END WITH
@rem   || exit /b 1
@rem BASICALLY, DO THE TESTING IN BASH...

@rem Do not call setlocal!
@echo off

goto %1

goto :EOF

:CheckPackage
"%CYG_ROOT%\bin\bash.exe" -lc "cygcheck -dc %1" | findstr %1 > nul
if %ERRORLEVEL% equ 1 (
  echo Cygwin package %1 will be installed
  set CYGWIN_INSTALL_PACKAGES=%CYGWIN_INSTALL_PACKAGES%,%1
)
goto :EOF

:UpgradeCygwin
if %CYGWIN_UPGRADE_REQUIRED% equ 1 (
  echo Cygwin package upgrade required - please go and drink coffee
  set CYGWIN_UPGRADE_FLAG=--upgrade-also
  SET CYGWIN_UPGRADE_REQUIRED=0
) else (
  set CYGWIN_UPGRADE_FLAG=
)
if "%CYGWIN_INSTALL_PACKAGES%" neq "" set CYGWIN_INSTALL_PACKAGES=--packages %CYGWIN_INSTALL_PACKAGES:~1%
if "%CYGWIN_INSTALL_PACKAGES%%FLAG%" equ "" goto UpgradeCygwin_next
pushd %CYG_ROOT%
if exist setup-%CYG_ARCH%.exe del setup-%CYG_ARCH%.exe
appveyor DownloadFile "https://cygwin.com/setup-%CYG_ARCH%.exe" -FileName "setup-%CYG_ARCH%.exe" || exit /b 1
popd
"%CYG_ROOT%\setup-%CYG_ARCH%.exe" --quiet-mode --no-shortcuts --no-startmenu --no-desktop --only-site --root "%CYG_ROOT%" --site "%CYG_MIRROR%" --local-package-dir "%CYG_CACHE%" %CYGWIN_INSTALL_PACKAGES% %CYGWIN_UPGRADE_FLAG% > nul
set CYGWIN_INSTALL_PACKAGES=
:UpgradeCygwin_next
if "%CYGWIN_UPGRADE_FLAG%" equ "" for %%P in (%CYGWIN_COMMANDS%) do "%CYG_ROOT%\bin\bash.exe" -lc "%%P --help" > nul || set CYGWIN_UPGRADE_REQUIRED=1
"%CYG_ROOT%\bin\bash.exe" -lc "cygcheck -dc %CYGWIN_PACKAGES%"
if "%CYGWIN_UPGRADE_REQUIRED%%CYGWIN_UPGRADE_FLAG%" equ "1" call :UpgradeCygwin
goto :EOF

:install
echo Build Worker Image: %APPVEYOR_BUILD_WORKER_IMAGE%
systeminfo 2>nul | findstr /B /C:"OS Name" /C:"OS Version"
echo System architecture: %PLATFORM%
set CYG_ROOT=C:\%CYG_ROOT%

cd "%APPVEYOR_BUILD_FOLDER%"

:: if "%OCAML_PORT%" equ "" (
::   rem Need unreleased Cygwin 3.1.7 for bugfix in acl_get_tag_type and acl_get_permset
::   appveyor DownloadFile "https://cygwin.com/snapshots/x86/cygwin1-20200710.dll.xz" -FileName "cygwin1.dll.xz" || exit /b 1
::   "%CYG_ROOT%\bin\bash.exe" -lc "cd $APPVEYOR_BUILD_FOLDER ; unxz cygwin1.dll.xz ; chmod +x cygwin1.dll"
::   move cygwin1.dll %CYG_ROOT%\bin\cygwin1.dll
:: )

rem CYGWIN_PACKAGES is the list of required Cygwin packages (cygwin is included
rem in the list just so that the Cygwin version is always displayed on the log).
rem CYGWIN_COMMANDS is a corresponding command to run with --version to test
rem whether the package works. This is used to verify whether the installation
rem needs upgrading.
set CYGWIN_PACKAGES=cygwin make patch curl diffutils tar unzip git
set CYGWIN_COMMANDS=cygcheck make patch curl diff tar unzip git

if "%OCAML_PORT%" equ "mingw" (
  set CYGWIN_PACKAGES=%CYGWIN_PACKAGES% mingw64-i686-gcc-g++
  set CYGWIN_COMMANDS=%CYGWIN_COMMANDS% i686-w64-mingw32-g++
)
if "%OCAML_PORT%" equ "mingw64" (
  set CYGWIN_PACKAGES=%CYGWIN_PACKAGES% mingw64-x86_64-gcc-g++
  set CYGWIN_COMMANDS=%CYGWIN_COMMANDS% x86_64-w64-mingw32-g++
)
if "%OCAML_PORT%" equ "" (
  set CYGWIN_PACKAGES=%CYGWIN_PACKAGES% gcc-g++ flexdll
  set CYGWIN_COMMANDS=%CYGWIN_COMMANDS% g++ flexlink
)

set CYGWIN_INSTALL_PACKAGES=
set CYGWIN_UPGRADE_REQUIRED=0

rem Check that all packages are installed
for %%P in (%CYGWIN_PACKAGES%) do call :CheckPackage %%P

rem Check that Cygwin is at least 3.1.7
for /f "tokens=2,3,4 delims=-. " %%a in ('%CYG_ROOT%\bin\bash.exe -lc "cygcheck -dc cygwin" ^| findstr cygwin') do (
  set CYG_MAJOR=%%a
  set CYG_MINOR=%%b
  set CYG_REV=%%c
)
set /a CYG_VER=%CYG_MAJOR%*10000+%CYG_MINOR%*100+%CYG_REV%
if %CYG_VER% lss 30107 (
  if "%OCAML_PORT%" equ "" (
    echo Cygwin version %CYG_MAJOR%.%CYG_MINOR%.%CYG_REV% installed; opam requires 3.1.7 or later
    set CYGWIN_UPGRADE_REQUIRED=1
  )
)

rem Upgrade/install packages as necessary
call :UpgradeCygwin

set INSTALLED_URL=
for /f "tokens=3" %%U in ('findstr /C:"URL_ocaml = " src_ext\Makefile') do set OCAML_URL=%%U
for /f "tokens=3" %%U in ('findstr /C:"URL_flexdll = " src_ext\Makefile') do set FLEXDLL_URL=%%U
if exist bootstrap\ocaml\lib\stdlib.cmxa (
  echo Deleting out-of-date bootstrap compiler
  rd /s/q bootstrap
)
if exist bootstrap\installed-tarball for /f "delims=" %%U in ('type bootstrap\installed-tarball') do set INSTALLED_URL=%%U

if "%INSTALLED_URL%" neq "%OCAML_URL% %FLEXDLL_URL% %DEP_MODE%" if exist bootstrap\nul (
  echo Required: %OCAML_URL% %FLEXDLL_URL% %DEP_MODE%
  echo Compiled: %INSTALLED_URL%
  echo Re-building bootstrap compiler
  rd /s/q bootstrap
  if exist src_ext\archives\nul rd /s/q src_ext\archives
)

if "%DEP_MODE%" equ "lib-pkg" "%CYG_ROOT%\bin\bash.exe" -lc "cd $APPVEYOR_BUILD_FOLDER && make --no-print-directory -C src_ext lib-pkg-urls | head -n -1 | sort | uniq" > current-lib-pkg-list
if not exist bootstrap\installed-packages goto SkipCheck

fc bootstrap\installed-packages current-lib-pkg-list > nul
if %ERRORLEVEL% equ 1 (
  echo lib-pkg packages changed:
  "%CYG_ROOT%\bin\diff.exe" bootstrap/installed-packages current-lib-pkg-list | "%CYG_ROOT%\bin\sed.exe" -ne "s/</Remove/p" -e "s/>/Add/p" | "%CYG_ROOT%\bin\gawk.exe" "BEGIN{FS="" ""}$2!=k{if(k!="""")print o==f?w:o;w=$0;k=$2;f=o=$2"" ""$3;next}{o=""Switch ""o"" --> ""$3}END{print o==f?w:o}"
  echo lib-pkg will be re-built
  "%CYG_ROOT%\bin\bash.exe" -lc "cd $APPVEYOR_BUILD_FOLDER && make --no-print-directory -C src_ext reset-lib-pkg"
  rd /s/q bootstrap
  if exist src_ext\archives\nul rd /s/q src_ext\archives
) else (
  del current-lib-pkg-list
)

:SkipCheck

"%CYG_ROOT%\bin\bash.exe" -lc "cd $APPVEYOR_BUILD_FOLDER && make --no-print-directory -C src_ext cache-archives" || exit /b 1

set RECOMPILED=0
if not exist bootstrap\nul (
  set RECOMPILED=1
  "%CYG_ROOT%\bin\bash.exe" -lc "cd $APPVEYOR_BUILD_FOLDER && make compiler" || exit /b 1
  for /f "delims=" %%U in ('type bootstrap\installed-tarball') do echo %%U %DEP_MODE%> bootstrap\installed-tarball
  if exist bootstrap\ocaml-*.tar.gz del bootstrap\ocaml-*.tar.gz
  if "%OCAML_PORT%" neq "" if exist bootstrap\flexdll-*.tar.gz del bootstrap\flexdll-*.tar.gz
  del bootstrap\ocaml\bin\*.byte.exe
  del bootstrap\ocaml\lib\ocaml\expunge.exe
  for /f %%D in ('dir /b/ad bootstrap\ocaml-*') do (
    rd /s/q bootstrap\%%D
    rem Directory needs to exist, as the Cygwin bootstraps OCAMLLIB refers to it
    rem and bootstrap-ocaml.sh assumes it will exist even when regenerating the
    rem config.
    md bootstrap\%%D
  )
) else (
  if not exist bootstrap\installed-packages "%CYG_ROOT%\bin\bash.exe" -lc "cd $APPVEYOR_BUILD_FOLDER && make --no-print-directory -C src_ext reset-lib-pkg"
  if exist current-lib-pkg-list "%CYG_ROOT%\bin\bash.exe" -lc "cd $APPVEYOR_BUILD_FOLDER && GEN_CONFIG_ONLY=1 shell/bootstrap-ocaml.sh %OCAML_PORT%" || exit /b 1
)

if exist current-lib-pkg-list (
  set RECOMPILED=1
  "%CYG_ROOT%\bin\bash.exe" -lc "cd $APPVEYOR_BUILD_FOLDER && make lib-pkg" || exit /b 1
  move current-lib-pkg-list bootstrap\installed-packages
)

goto :EOF

:build
if "%OCAML_PORT%" equ "" (
  rem make install doesn't yet work for the native Windows builds
  set POST_COMMAND=^&^& make opam-installer install
)
set LIB_EXT=
if "%DEP_MODE%" equ "lib-ext" set LIB_EXT=^&^& make lib-ext
set PRIVATE_RUNTIME=
if "%OCAML_PORT:~0,5%" equ "mingw" set PRIVATE_RUNTIME=--with-private-runtime
set WITH_MCCS=--with-mccs
if "%DEP_MODE%" equ "lib-pkg" set WITH_MCCS=
"%CYG_ROOT%\bin\bash.exe" -lc "cd $APPVEYOR_BUILD_FOLDER %LIB_PKG% && ./configure %PRIVATE_RUNTIME% %WITH_MCCS% %LIB_EXT%" || exit /b 1
"%CYG_ROOT%\bin\bash.exe" -lc "cd $APPVEYOR_BUILD_FOLDER && echo DUNE_PROFILE=dev >> Makefile.config" || exit /b 1
"%CYG_ROOT%\bin\bash.exe" -lc "cd $APPVEYOR_BUILD_FOLDER && make opam %POST_COMMAND%" || exit /b 1
goto :EOF

:test
if %RECOMPILED% equ 1 (
  echo Testing skipped for this run to avoid timeout
  goto :EOF
)
rem Configure Git for Windows (for the testsuite, this isn't strictly necessary
rem as Git-for-Windows will pick up $HOME/.gitconfig for Cygwin's git)
git config --global user.email travis@example.com
git config --global user.name Travis
rem Configure Cygwin's Git
"%CYG_ROOT%\bin\bash.exe" -lc "git config --global user.email travis@example.com"
"%CYG_ROOT%\bin\bash.exe" -lc "git config --global user.name Travis"
set OPAMCOLOR=always
set PATH_SHIM=
if "%OCAML_PORT%" neq "" if "%GIT_FOR_WINDOWS%" equ "1" (
  set PATH_SHIM=PATH=/cygdrive/c/Program\ Files/Git/cmd:$PATH
  "C:\Program Files\Git\cmd\git.exe" config --global core.autocrlf
  "C:\Program Files\Git\cmd\git.exe" config --global core.autocrlf true
  "C:\Program Files\Git\cmd\git.exe" config --global core.autocrlf
)
"%CYG_ROOT%\bin\bash.exe" -lc "%PATH_SHIM% make -C $APPVEYOR_BUILD_FOLDER tests" || exit /b 1
rem Can't yet do an opam init with the native Windows builds
if "%OCAML_PORT%" equ "" "%CYG_ROOT%\bin\bash.exe" -lc "make -C $APPVEYOR_BUILD_FOLDER run-appveyor-test" || exit /b 1
goto :EOF
