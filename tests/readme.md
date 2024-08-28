# Opam test suite

There is 3 different kind of tests
* CRAM test in `reftests`. It is used to check opam behaviour. See
  `reftests/readme.md` for more details.
* Bench tests in `bench`, using
  [current-bench](https://github.com/ocurrent/current-bench). It is used to
  benchmark some specific operations.
* OCaml tests in `lib`. For the moment it is used to check some type
  manipulation and patch code.

There is also other tests:
* [`opam-rt`](https://github.com/ocaml-opam/opam-rt): historical test suite. it
  is a mix of opam cli calls and library usage. It is meant to be removed once
  all test has been ported to `reftests` and `lib`
* [`opam-crowbar`](https://github.com/ocaml/opam/blob/master/src/crowbar/opam-crowbar.opam):
  some fuzz testing of some opam library main types.
