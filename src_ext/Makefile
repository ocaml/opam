FETCH ?= curl -OL

all: clone depends.ocp

depends.ocp: depends.ocp.boot
	cp $< $@

clone: cudf.stamp extlib.stamp ocaml-re.stamp ocamlgraph.stamp dose.stamp ocaml-arg.stamp

cudf.tar.gz:
	$(FETCH) http://www.ocamlpro.com/pub/cudf.tar.gz

extlib-1.5.3.tar.gz:
	$(FETCH) http://ocaml-extlib.googlecode.com/files/extlib-1.5.3.tar.gz
	@touch $@

cudf.stamp: cudf.tar.gz
	tar xfz cudf.tar.gz
	@touch $@

dose.tar.gz:
	$(FETCH) http://www.ocamlpro.com/pub/dose.tar.gz

ocaml-arg.0.2.tar.gz:
	$(FETCH) http://www.ocamlpro.com/pub/ocaml-arg.0.2.tar.gz

ocamlgraph-1.8.1.tar.gz:
	$(FETCH) http://ocamlgraph.lri.fr/download/ocamlgraph-1.8.1.tar.gz

ocaml-re.1.1.tar.gz:	
	$(FETCH) http://www.ocamlpro.com/pub/ocaml-re.1.1.tar.gz

extlib.stamp: extlib-1.5.3.tar.gz
	tar xfz extlib-1.5.3.tar.gz
	mv extlib-1.5.3 extlib
	@touch $@

dose.stamp: dose.tar.gz
	tar xfz dose.tar.gz
	@touch $@

ocaml-arg.stamp: ocaml-arg.0.2.tar.gz
	tar xfz ocaml-arg.0.2.tar.gz
	@touch $@

ocamlgraph.stamp: ocamlgraph-1.8.1.tar.gz
	tar xfz ocamlgraph-1.8.1.tar.gz
	mv ocamlgraph-1.8.1 ocamlgraph
	@touch $@

ocaml-re.stamp: ocaml-re.1.1.tar.gz
	tar xfz ocaml-re.1.1.tar.gz
	@touch $@

clean:
	rm -rf *~

distclean: clean
	rm -rf cudf extlib ocaml-re ocamlgraph dose ocaml-arg
	rm -f depends.ocp
	rm -f *.tar.gz *.stamp
