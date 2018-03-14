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
if "%CYGWIN_INSTALL_PACKAGES%" neq "" "%CYG_ROOT%\setup-%CYG_ARCH%.exe" --quiet-mode --no-shortcuts --no-startmenu --no-desktop --only-site --root "%CYG_ROOT%" --site "%CYG_MIRROR%" --local-package-dir "%CYG_CACHE%" --packages %CYGWIN_INSTALL_PACKAGES:~1% > nul
for %%P in (%CYGWIN_COMMANDS%) do "%CYG_ROOT%\bin\bash.exe" -lc "%%P --help" > nul || set CYGWIN_UPGRADE_REQUIRED=1
"%CYG_ROOT%\bin\bash.exe" -lc "cygcheck -dc %CYGWIN_PACKAGES%"
if %CYGWIN_UPGRADE_REQUIRED% equ 1 (
  echo Cygwin package upgrade required - please go and drink coffee
  "%CYG_ROOT%\setup-%CYG_ARCH%.exe" --quiet-mode --no-shortcuts --no-startmenu --no-desktop --only-site --root "%CYG_ROOT%" --site "%CYG_MIRROR%" --local-package-dir "%CYG_CACHE%" --upgrade-also > nul
  "%CYG_ROOT%\bin\bash.exe" -lc "cygcheck -dc %CYGWIN_PACKAGES%"
)
goto :EOF

:install
set CYG_ROOT=C:\%CYG_ROOT%

cd "%APPVEYOR_BUILD_FOLDER%"

rem CYGWIN_PACKAGES is the list of required Cygwin packages (cygwin is included
rem in the list just so that the Cygwin version is always displayed on the log).
rem CYGWIN_COMMANDS is a corresponding command to run with --version to test
rem whether the package works. This is used to verify whether the installation
rem needs upgrading.
set CYGWIN_PACKAGES=cygwin make patch curl diffutils tar unzip
set CYGWIN_COMMANDS=cygcheck make patch curl diff tar unzip

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

for %%P in (%CYGWIN_PACKAGES%) do call :CheckPackage %%P
call :UpgradeCygwin

rem Use dra27 jbuilder/ocaml-mccs/flexdll for native ports
if "%OCAML_PORT%" neq "" git apply appveyor.patch

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
  del bootstrap\installed-packages
) else (
  del current-lib-pkg-list
)

:SkipCheck

"%CYG_ROOT%\bin\bash.exe" -lc "cd $APPVEYOR_BUILD_FOLDER && make --no-print-directory -C src_ext cache-archives" || exit /b 1

if not exist bootstrap\nul (
  "%CYG_ROOT%\bin\bash.exe" -lc "cd $APPVEYOR_BUILD_FOLDER && make compiler" || exit /b 1
  for /f "delims=" %%U in ('type bootstrap\installed-tarball') do echo %%U %DEP_MODE%> bootstrap\installed-tarball
  if "%CYG_ARCH%%OCAML_PORT%" equ "x86_64" (
    "%CYG_ROOT%\bin\bash.exe" -lc "cd $APPVEYOR_BUILD_FOLDER && rebase -b 0x7cd20000 bootstrap/ocaml/lib/ocaml/stublibs/dllunix.so" || exit /b 1
    "%CYG_ROOT%\bin\bash.exe" -lc "cd $APPVEYOR_BUILD_FOLDER && rebase -b 0x7cd20000 bootstrap/ocaml/lib/ocaml/stublibs/dllthreads.so" || exit /b 1
  )
  if exist bootstrap\ocaml-*.tar.gz del bootstrap\ocaml-*.tar.gz
  if "%OCAML_PORT%" neq "" if exist bootstrap\flexdll-*.tar.gz del bootstrap\flexdll-*.tar.gz
  del bootstrap\ocaml\bin\*.byte.exe
  if "%OCAML_PORT%" equ "" (
    del bootstrap\ocaml\lib\ocaml\expunge.exe
  ) else (
    del bootstrap\ocaml\lib\expunge.exe
  )
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
"%CYG_ROOT%\bin\bash.exe" -lc "cd $APPVEYOR_BUILD_FOLDER %LIB_PKG% && ./configure %LIB_EXT% && make opam %POST_COMMAND%" || exit /b 1
goto :EOF

:test
if "%OCAML_PORT%" neq "" (
  echo Running the opam command...
  opam || exit /b 1
)
rem Can't yet do an opam init with the native Windows builds
if "%OCAML_PORT%" equ "" "%CYG_ROOT%\bin\bash.exe" -lc "make -C $APPVEYOR_BUILD_FOLDER run-appveyor-test" || exit /b 1
goto :EOF
