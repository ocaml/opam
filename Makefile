OCPBUILD ?= ./_obuild/unixrun ./boot/ocp-build.boot
OCAMLC=ocamlc
SRC_EXT=src_ext

TARGET = ocp-get ocp-get-server

.PHONY: all

all: ./_obuild/unixrun compile link clone
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

bootstrap: _obuild/unixrun _obuild/ocp-get/ocp-get.byte
	rm -f boot/ocp-get.boot
	ocp-bytehack -static _obuild/ocp-get/ocp-get.byte -o boot/ocp-get.boot

link: ocp-get ocp-get-server
	@

_obuild/ocp-get-server/ocp-get-server.asm:
	$(OCPBUILD) ocp-get-server

_obuild/ocp-get/ocp-get.asm:
	$(OCPBUILD) ocp-get

ocp-get-server: _obuild/ocp-get-server/ocp-get-server.asm
	ln -s $^ ocp-get-server

ocp-get: _obuild/ocp-get/ocp-get.asm
	ln -s $^ ocp-get

compile: ./_obuild/unixrun clone
	$(OCPBUILD) -init -scan -sanitize $(TARGET)


clone: 
	$(MAKE) -C $(SRC_EXT)

clean:
	rm -rf _obuild
	rm -rf src/*.annot bat/*.annot
	rm -f ocp-get ocp-get-server
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
