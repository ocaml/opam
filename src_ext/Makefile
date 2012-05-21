OS = $(shell uname -s)
ifeq ($(OS),Darwin)
WGET ?= ftp
else
WGET ?= wget -qN
endif

all: clone depends.ocp

depends.ocp: depends.ocp.boot
	cp $< $@

clone: cudf extlib ocaml-re ocamlgraph dose ocaml-arg

cudf:
	$(WGET) http://www.ocamlpro.com/pub/cudf.tar.gz
	tar xfz cudf.tar.gz

extlib:
	$(WGET) http://ocaml-extlib.googlecode.com/files/extlib-1.5.2.tar.gz
	tar xfz extlib-1.5.2.tar.gz
	mv extlib-1.5.2 extlib

dose:
	$(WGET) http://www.ocamlpro.com/pub/dose.tar.gz
	tar xfz dose.tar.gz

ocaml-arg:
	$(WGET) http://www.ocamlpro.com/pub/ocaml-arg.tar.bz2
	tar xfj ocaml-arg.tar.bz2

ocamlgraph:
	$(WGET) http://ocamlgraph.lri.fr/download/ocamlgraph-1.8.1.tar.gz
	tar xfz ocamlgraph-1.8.1.tar.gz
	mv ocamlgraph-1.8.1 ocamlgraph

ocaml-re:
	$(WGET) http://www.ocamlpro.com/pub/ocaml-re.tar.gz
	tar xfz ocaml-re.tar.gz

clean:
	rm -rf *~

distclean: clean
	rm -rf cudf extlib ocaml-re ocamlgraph dose ocaml-arg
	rm -f depends.ocp
	rm -rf *.tar.gz
	rm -rf *.bz2