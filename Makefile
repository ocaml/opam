OCPBUILD ?= ./_obuild/unixrun ./boot/ocp-build.boot
WGET     ?= wget
TARGET   = ocp-get

.PHONY: all

all: ./_obuild/unixrun compile clone
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

compile: ./_obuild/unixrun clone
	$(OCPBUILD) -init -scan -sanitize $(TARGET)

clone: cudf extlib ocaml-pcre ocamlgraph dose

cudf:
	git clone git://scm.gforge.inria.fr/mancoosi-tools/cudf.git

extlib:
	$(WGET) http://ocaml-extlib.googlecode.com/files/extlib-1.5.2.tar.gz
	tar xfvz extlib-1.5.2.tar.gz
	mv extlib-1.5.2 extlib

dose:
	git clone git://scm.gforge.inria.fr/mancoosi-tools/dose.git

ocaml-pcre:
	$(WGET) http://hg.ocaml.info/release/pcre-ocaml/archive/release-6.2.5.tar.gz
	tar xfvz release-6.2.5.tar.gz
	mv pcre-ocaml-release-6.2.5 ocaml-pcre

ocamlgraph:
	$(WGET) http://ocamlgraph.lri.fr/download/ocamlgraph-1.8.1.tar.gz
	tar xvfz ocamlgraph-1.8.1.tar.gz
	mv ocamlgraph-1.8.1 ocamlgraph

clean:
	$(OCPBUILD) -clean

# ocaml-re:
# 	git clone https://github.com/avsm/ocaml-re

distclean:
	rm -f *.tar.gz
	rm -rf dose cudf extlib ocaml-pcre ocamlgraph
	rm -rf _obuild