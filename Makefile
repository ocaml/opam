OCPBUILD ?= ./_obuild/unixrun ./boot/ocp-build.boot
WGET     ?= wget
TARGET   = ocp-get ocp-get-server

.PHONY: all

all: ./_obuild/unixrun compile clone link
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
	ocamlc -o ./_obuild/unixrun -make-runtime unix.cma

link: ocp-get ocp-get-server
	@

ocp-get:
	ln -s _obuild/ocp-get/ocp-get.asm ocp-get

ocp-get-server:
	ln -s _obuild/ocp-get-server/ocp-get-server.asm ocp-get-server

compile: ./_obuild/unixrun clone
	$(OCPBUILD) -init -scan -sanitize $(TARGET)

clone: cudf extlib ocaml-re ocamlgraph dose

cudf:
	$(WGET) http://www.ocamlpro.com/pub/cudf.tar.bz2
	tar xfvj cudf.tar.bz2

extlib:
	$(WGET) http://ocaml-extlib.googlecode.com/files/extlib-1.5.2.tar.gz
	tar xfvz extlib-1.5.2.tar.gz
	mv extlib-1.5.2 extlib

dose:
	$(WGET) http://www.ocamlpro.com/pub/dose.tar.bz2
	tar xvfz dose.tar.bz2

# ocaml-pcre:
# 	$(WGET) http://hg.ocaml.info/release/pcre-ocaml/archive/release-6.2.5.tar.gz
# 	tar xfvz release-6.2.5.tar.gz
# 	mv pcre-ocaml-release-6.2.5 ocaml-pcre

ocamlgraph:
	$(WGET) http://ocamlgraph.lri.fr/download/ocamlgraph-1.8.1.tar.gz
	tar xvfz ocamlgraph-1.8.1.tar.gz
	mv ocamlgraph-1.8.1 ocamlgraph

clean:
	$(OCPBUILD) -clean
	rm -rf src/*.annot bat/*.annot
	rm -f ocp-get ocp-get-server

ocaml-re:
	$(WGET) http://www.ocamlpro.com/pub/ocaml-re.tar.gz
	tar xcfj ocaml-re.tar.gz

distclean:
	rm -f *.tar.gz
	rm -rf dose cudf extlib ocaml-pcre ocamlgraph
	rm -rf _obuild

.PHONY: tests
tests:
	make -C tests

tests-runserver:
	make -C tests runserver
