include ../Makefile.config

all: clone depends.ocp

depends.ocp: depends.ocp.boot
	cp $< $@

clone: cudf.stamp extlib.stamp ocaml-re.stamp ocamlgraph.stamp dose.stamp cmdliner.stamp

cudf-0.6.3.tar.gz:
	$(FETCH) -k https://gforge.inria.fr/frs/download.php/31910/cudf-0.6.3.tar.gz

cudf.stamp: cudf-0.6.3.tar.gz
	tar xfz cudf-0.6.3.tar.gz
	rm -rf cudf
	mv cudf-0.6.3 cudf
	@touch $@

extlib-1.5.3.tar.gz:
	$(FETCH) http://ocaml-extlib.googlecode.com/files/extlib-1.5.3.tar.gz

extlib.stamp: extlib-1.5.3.tar.gz
	tar xfz extlib-1.5.3.tar.gz
	rm -rf extlib
	mv extlib-1.5.3 extlib
	@touch $@

dose3-git:
	@if [ -e dose ]; then \
	  cd dose; \
	  git pull; \
	else \
	  git clone --depth 1 git://scm.gforge.inria.fr/dose/dose.git dose; \
	fi

dose3-3.1.2.tar.gz:
	$(FETCH) https://gforge.inria.fr/frs/download.php/31595/dose3-3.1.2.tar.gz

dose.stamp: dose3-3.1.2.tar.gz
	tar xfz dose3-3.1.2.tar.gz
	rm -rf dose
	mv dose3-3.1.2 dose
	@touch $@

cmdliner-0.9.2.tbz:
	$(FETCH) http://erratique.ch/software/cmdliner/releases/cmdliner-0.9.2.tbz

cmdliner.stamp: cmdliner-0.9.2.tbz
	tar xfj cmdliner-0.9.2.tbz
	rm -rf cmdliner
	mv cmdliner-0.9.2 cmdliner
	@touch $@

ocamlgraph-1.8.1.tar.gz:
	$(FETCH) http://ocamlgraph.lri.fr/download/ocamlgraph-1.8.1.tar.gz

ocamlgraph.stamp: ocamlgraph-1.8.1.tar.gz
	tar xfz ocamlgraph-1.8.1.tar.gz
	rm -rf ocamlgraph
	mv ocamlgraph-1.8.1 ocamlgraph
	@touch $@

ocaml-re.1.1.tar.gz:
	$(FETCH) http://www.ocamlpro.com/pub/ocaml-re.1.1.tar.gz

ocaml-re.stamp: ocaml-re.1.1.tar.gz
	tar xfz ocaml-re.1.1.tar.gz
	@touch $@

clean:
	rm -rf *~

distclean: clean
	rm -rf cudf extlib ocaml-re ocamlgraph dose cmdliner
	rm -f depends.ocp
	rm -f *.tar.gz *.tbz *.stamp
