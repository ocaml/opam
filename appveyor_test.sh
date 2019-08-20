#!/bin/bash

opam init -y -a
eval $(opam config env)
opam install -y -v ocamlfind
