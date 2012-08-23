BIN = /usr/local/bin
OCPBUILD ?= ./_obuild/unixrun ./boot/ocp-build.boot
OCAMLC=ocamlc
SRC_EXT=src_ext
TARGETS = opam opam-mk-repo opam-repo-convert-0.3

.PHONY: all

all: ./_obuild/unixrun
	$(MAKE) clone
	$(MAKE) compile

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

compile: ./_obuild/unixrun
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
	$(MAKE) -C tests all

tests-rsync:
	$(MAKE) -C tests rsync

tests-git:
	$(MAKE) -C tests git

%-install:
	cp _obuild/$*/$*.asm $(BIN)/$*

.PHONY: install
install:
	rm $(BIN)/opam*
	$(MAKE) $(TARGETS:%=%-install)

doc: compile
	mkdir -p doc/html/
	ocamldoc \
	  -I _obuild/opam-lib -I _obuild/cudf -I _obuild/dose \
	  -I _obuild/bat -I _obuild/unix -I _obuild/extlib \
	  -I _obuild/arg -I _obuild/graph \
	  src/*.mli -html -d doc/html/

trailing:
	find src -name "*.ml*" -exec \
	  sed -i xxx -e :a -e "/^\n*$$/{$$d;N;ba" -e '}' {} \;
	find src -name "*xxx" -exec rm {} \;
