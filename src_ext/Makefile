-include ../Makefile.config

CUDF    = cudf-0.6.3
CUDFMD5 = 40c4e2c50ea96d0c9e565db16d20639a

EXTLIB    = extlib-1.5.3
EXTLIBMD5 = 3de5f4e0a95fda7b2f3819c4a655b17c

DOSE     = dose3-3.1.2
DOSEMD5  = e98ff720fcc3873def46c85c6a980a1b

CMDLINER    = cmdliner-0.9.3
CMDLINERMD5 = d63dd3b03966d65fc242246859c831c7

GRAPH    = ocamlgraph-1.8.1
GRAPHMD5 = 5aa256e9587a6d264d189418230af698

RE       = ocaml-re-1.2.0
REMD5    = 5cbfc137683ef2b0e91f931577f2e673

# Portable md5check
MD5CHECK = ocaml ../shell/md5check.ml

all: clone depends.ocp

depends.ocp: depends.ocp.boot
	cp $< $@

clone: cudf.stamp extlib.stamp ocaml-re.stamp ocamlgraph.stamp dose.stamp cmdliner.stamp


$(CUDF).tar.gz:
	$(FETCH) https://gforge.inria.fr/frs/download.php/31910/$(CUDF).tar.gz
	$(MD5CHECK) $(CUDF).tar.gz $(CUDFMD5)

cudf.stamp: $(CUDF).tar.gz
	tar xfz $(CUDF).tar.gz
	rm -rf cudf
	mv $(CUDF) cudf
	@touch $@

$(EXTLIB).tar.gz:
	$(FETCH) http://ocaml-extlib.googlecode.com/files/$(EXTLIB).tar.gz
	$(MD5CHECK) $(EXTLIB).tar.gz $(EXTLIBMD5)

extlib.stamp: $(EXTLIB).tar.gz
	tar xfz $(EXTLIB).tar.gz
	rm -rf extlib
	mv $(EXTLIB) extlib
	@touch $@

dose3-git:
	@if [ -e dose ]; then \
	  cd dose; \
	  git pull; \
	else \
	  git clone --depth 1 git://scm.gforge.inria.fr/dose/dose.git dose; \
	fi

$(DOSE).tar.gz:
	$(FETCH) https://gforge.inria.fr/frs/download.php/31595/$(DOSE).tar.gz
	$(MD5CHECK) $(DOSE).tar.gz $(DOSEMD5)

dose.stamp: $(DOSE).tar.gz
	tar xfz $(DOSE).tar.gz
	cd $(DOSE) && patch -p1  < ../dose-pcre2re.diff && cd ..
	rm -rf dose
	mv $(DOSE) dose
	@touch $@

$(CMDLINER).tbz:
	$(FETCH) http://erratique.ch/software/cmdliner/releases/$(CMDLINER).tbz
	$(MD5CHECK) $(CMDLINER).tbz $(CMDLINERMD5)

cmdliner.stamp: $(CMDLINER).tbz
	tar xfj $(CMDLINER).tbz
	rm -rf cmdliner
	mv $(CMDLINER) cmdliner
	@touch $@

$(GRAPH).tar.gz:
	$(FETCH) http://ocamlgraph.lri.fr/download/$(GRAPH).tar.gz
	$(MD5CHECK) $(GRAPH).tar.gz $(GRAPHMD5)

ocamlgraph.stamp: $(GRAPH).tar.gz
	tar xfz $(GRAPH).tar.gz
	rm -rf ocamlgraph
	mv $(GRAPH) ocamlgraph
	@touch $@

$(RE).tar.gz:
	$(FETCH) https://github.com/ocaml/ocaml-re/archive/$(RE).tar.gz
	$(MD5CHECK) $(RE).tar.gz $(REMD5)

ocaml-re.stamp: $(RE).tar.gz
	tar xfz $(RE).tar.gz
	mv ocaml-re-$(RE) ocaml-re
	@touch $@

clean:
	rm -rf *~

distclean: clean
	rm -rf cudf extlib ocaml-re ocamlgraph dose cmdliner
	rm -f depends.ocp
	rm -f *.tar.gz *.tbz *.stamp
