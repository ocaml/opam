BIN = /usr/local/bin
OCPBUILD ?= ./_obuild/unixrun ./boot/ocp-build.boot
OCAMLC=ocamlc
SRC_EXT=src_ext
TARGETS = opam opam-server \
	  opam-rsync-init opam-rsync-update opam-rsync-download opam-rsync-upload \
	  opam-git-init opam-git-update opam-git-download opam-git-upload \
	  opam-server-init opam-server-update opam-server-download opam-server-upload \
	  opam-mk-config opam-mk-install opam-mk-repo

.PHONY: all

all: ./_obuild/unixrun compile clone build
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

build:
	$(OCPBUILD) $(TARGETS)

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

.PHONY: tests tests-server
tests:
	$(MAKE) -C tests

tests-server:
	$(MAKE) -C tests server

tests-git:
	$(MAKE) -C tests git

%-install:
	cp _obuild/$*/$*.asm $(BIN)/$*

.PHONY: install
install: $(TARGETS:%=%-install)
	@

doc: compile
	mkdir -p html/
	ocamldoc \
	  -I _obuild/opam-lib -I _obuild/cudf -I _obuild/dose \
	  -I _obuild/bat -I _obuild/unix -I _obuild/extlib \
	  -I _obuild/arg -I _obuild/graph \
	  src/*.mli -html -d html/