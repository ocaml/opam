# Installing packages

[This information is expected to be primarily of use when compiling on Windows]

`make lib-pkg` is supposed to be used only when a bootstrap compiler has been built using `make compiler` and will display an error if it run when no bootstrap directory exists. However it is possible to install missing packages in a system compiler (assuming appropriate privileges, etc.) by running:

`make [-j] clone-pkg`

to download and assemble the build directories. You may then touch any `.pkgbuild` to indicate which packages are *not* required (e.g. touch findlib.pkgbuild, if the system compiler already has ocamlfind) and then run:

`make [-j] build-pkg`