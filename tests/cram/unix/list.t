Test https://github.com/ocaml/opam/issues/4216
  $ export OPAMROOT=./opamroot
  $ opam init --bare -n git+https://github.com/ocaml/opam-repository.git#0ec9bbf73a91b8e90b15bb11735859c9552a9888 > /dev/null
  $ OCAMLRUNPARAM=b opam list --depends-on dune -s --all-versions | head -n1
  0install.2.14
  Fatal error: exception Sys_error("Broken pipe")
  Raised by primitive operation at OpamCliMain.main_catch_all in file "src/client/opamCliMain.ml", line 357, characters 4-16
  Called from Dune__exe__OpamMain in file "src/client/opamMain.ml", line 12, characters 9-28
