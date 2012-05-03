OCPBUILD ?= ./_obuild/unixrun ./boot/ocp-build.boot
OCAMLC=ocamlc
SRC_EXT=src_ext

TARGETS = opam

.PHONY: all

all: ./_obuild/unixrun compile clone $(TARGETS)
	@

scan: ./_obuild/unixrun
	$(OCPBUILD) -scan
sanitize: ./_obuild/unixrun
	$(OCPBUILD) -sanitize
byte: ./_obuild/unixrun
	$(OCPBUILD) -byte
opt: ./_obuild/unixrun
	$(OCPBUILD) -asm
./_obuild/unixrun:
	mkdir -p ./_obuild
	$(OCAMLC) -o ./_obuild/unixrun -make-runtime unix.cma str.cma

bootstrap: _obuild/unixrun _obuild/opam/opam.byte
	rm -f boot/opam.boot
	ocp-bytehack -static _obuild/opam/opam.byte -o boot/opam.boot

opam:
	$(OCPBUILD) opam

compile: ./_obuild/unixrun clone
	$(OCPBUILD) -init -scan -sanitize $(TARGET)


clone: 
	$(MAKE) -C $(SRC_EXT)

clean:
	rm -rf _obuild
	rm -rf src/*.annot bat/*.annot
	rm -f opam
	rm -f ocp-build.*
	$(MAKE) -C $(SRC_EXT) clean

distclean: clean
	rm -f *.tar.gz *.tar.bz2
	rm -rf _obuild _build
	$(MAKE) -C $(SRC_EXT) distclean

.PHONY: tests
tests:
	$(MAKE) -C tests

tests-runserver:
	$(MAKE) -C tests runserver
