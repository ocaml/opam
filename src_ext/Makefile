-include ../Makefile.config

SRC_EXTS = extlib re cmdliner graph cudf dose uutf jsonm

URL_extlib = http://ocaml-extlib.googlecode.com/files/extlib-1.5.3.tar.gz
MD5_extlib = 3de5f4e0a95fda7b2f3819c4a655b17c

URL_re = https://github.com/ocaml/ocaml-re/archive/ocaml-re-1.2.0.tar.gz
MD5_re = 5cbfc137683ef2b0e91f931577f2e673

URL_cmdliner = http://erratique.ch/software/cmdliner/releases/cmdliner-0.9.7.tbz
MD5_cmdliner = 46d7553b95f623f811df2a844fb448fb

URL_graph = http://ocamlgraph.lri.fr/download/ocamlgraph-1.8.5.tar.gz
MD5_graph = 75dde65bfc3f9b07e795343d369aa84d

URL_cudf = http://opam.ocaml.org/archives/cudf.0.7+opam.tar.gz
MD5_cudf = 2c23f4a6c377270c34962574a1bed669

URL_dose = http://opam.ocaml.org/archives/dose.3.3+opam.tar.gz
MD5_dose = 27798238d02777e4d1ccd032b3189421

URL_uutf = http://erratique.ch/software/uutf/releases/uutf-0.9.3.tbz
MD5_uutf = 708c0421e158b390c7cc341f37b40add

URL_jsonm = http://erratique.ch/software/jsonm/releases/jsonm-0.9.1.tbz
MD5_jsonm = 631a5dabdada83236c83056f60e42685

ARCHIVES = $(foreach lib,$(SRC_EXTS),$(notdir $(URL_$(lib))))
lib_of = $(foreach lib,$(SRC_EXTS),$(if $(findstring $(1),$(URL_$(lib))),$(lib),,))

# Portable md5check
MD5CHECK = $(OCAML) ../shell/md5check.ml

lib-ext: clone build copy
	@

build: clone
	[ $(OCAMLC) = no ] || $(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBTARGET=bcl
	[ $(OCAMLOPT) = no ] || $(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBTARGET=ncl

all: bcl ncl

clone: $(SRC_EXTS:=.stamp) graph-workaround
	@

archives: $(SRC_EXTS:=.download)
	@

%.download:
	[ -e  $(notdir $(URL_$*)) ] || $(FETCH) $(URL_$*)
	$(MD5CHECK) $(notdir $(URL_$*)) $(MD5_$*)

%.stamp: %.download
	mkdir -p tmp
	cd tmp && tar xf$(if $(patsubst %.tar.gz,,$(URL_$*)),j,z) ../$(notdir $(URL_$*))
	rm -rf $*
	@for ii in tmp/*; do if [ -d $${ii} ]; then mv $${ii} $*; fi; done; \
	rm -rf tmp
	@if [ -d patches/$* ]; then \
          cd $* && \
	  for p in ../patches/$*/*.patch; do \
	    patch -p1 < $$p; \
	  done; \
        fi
	@touch $@

# OCamlMakefile doesn't include stand-alone mlis in the packs...
graph-workaround:
	cp graph/src/sig.mli graph/src/sig.ml
	cp graph/src/sig_pack.mli graph/src/sig_pack.ml
	cp graph/src/dot_ast.mli graph/src/dot_ast.ml

clean:
	rm -rf lib
	$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBTARGET=cleanup

distclean:
	rm -rf cudf extlib re graph dose cmdliner uutf jsonm
	rm -f depends.ocp
	rm -f *.tar.gz *.tbz *.stamp
	rm -f *.cm* *.o *.a *.lib
	rm -rf lib

LIB_EXTS = extlib re cmdliner graph cudf dose_common dose_debian dose_algo uutf jsonm

inst_objs = cp $(1)/*.cm*i lib

copy: build
	mkdir -p lib
	cp *.cma lib || true
	cp *.cmxa lib || true
	cp *.a lib || true
	cp *.lib lib || true
	$(call inst_objs,extlib)
	$(call inst_objs,re/lib)
	$(call inst_objs,cmdliner/src)
	$(call inst_objs,graph/src)
	$(call inst_objs,cudf)
	$(call inst_objs,dose/common)
	$(call inst_objs,dose/deb)
	$(call inst_objs,dose/algo)
	$(call inst_objs,uutf/src)
	$(call inst_objs,jsonm/src)
	$(call inst_objs,.)

# --

addmli = $(foreach ml,$(2),$(wildcard $(addsuffix .mli,$(basename $(1)/$(ml)))) $(1)/$(ml))

SRC_extlib = enum.ml extHashtbl.ml extList.ml extString.ml global.ml install.ml	\
IO.ml option.ml pMap.ml refList.ml std.ml uChar.ml unzip.ml uTF8.ml optParse.ml	\
dynArray.ml dllist.ml bitSet.ml base64.ml extArray.ml extLib.ml

define PROJ_extlib
  SOURCES = $(call addmli,extlib,$(SRC_extlib))
  RESULT = extlib
  # Don't omit !! it leaks between subprojs...
  LIB_PACK_NAME =
endef
export PROJ_extlib

SRC_re = cset.ml automata.ml re.ml re_perl.ml re_pcre.ml re_emacs.ml re_glob.ml	\
re_str.ml

define PROJ_re
  SOURCES = $(call addmli,re/lib,$(SRC_re))
  RESULT = re
  LIB_PACK_NAME =
endef
export PROJ_re

SRC_cmdliner = cmdliner.ml

define PROJ_cmdliner
  SOURCES = $(call addmli,cmdliner/src,$(SRC_cmdliner))
  RESULT = cmdliner
  LIB_PACK_NAME =
endef
export PROJ_cmdliner

SRC_graph_lib = bitv.ml heap.ml unionfind.ml
SRC_graph = sig_pack.ml sig.ml dot_ast.ml builder.ml classic.ml contraction.ml	\
delaunay.ml dominator.ml dot_parser.mly fixpoint.ml gmap.ml gml.mll graphml.ml	\
graphviz.ml leaderlist.ml oper.ml rand.ml strat.ml traverse.ml util.ml		\
version.ml kruskal.ml flow.ml dot_lexer.mll components.ml coloring.ml blocks.ml	\
dot.ml imperative.ml merge.ml minsep.ml persistent.ml nonnegative.ml path.ml	\
topological.ml pack.ml mcs_m.ml cliquetree.ml md.ml

define PROJ_graph
  SOURCES = $(call addmli,graph/lib,$(SRC_graph_lib)) $(call addmli,graph/src,$(SRC_graph))
  RESULT = graph
  LIB_PACK_NAME = graph
endef
export PROJ_graph

SRC_cudf = cudf_types.ml cudf_type_parser.mly cudf_type_lexer.mll cudf_conf.ml	\
cudf_822_parser.mly cudf_822_lexer.mll cudf_types_pp.ml main_cudf_parse_822.ml	\
cudf.ml cudf_checker.ml cudf_parser.mly cudf_printer.ml main_cudf_check.ml	\
cudf_c.ml

define PROJ_cudf
  SOURCES = $(call addmli,cudf,$(SRC_cudf))
  RESULT = cudf
  LIB_PACK_NAME =
  INCDIRS = extlib
endef
export PROJ_cudf

SRC_dose_common = util.ml url.ml input.ml edosSolver.ml cudfSolver.ml	\
cudfAdd.ml cudfDiff.ml

define PROJ_dose_common
  SOURCES = $(call addmli,dose/common,$(SRC_dose_common))
  RESULT = dose_common
  LIB_PACK_NAME = common
  INCDIRS = extlib cudf re/lib
endef
export PROJ_dose_common

SRC_dose_debian = architecture.ml format822.ml format822_parser.mly		\
packages_parser.mly version.ml packages_lexer.mll format822_lexer.mll		\
packages.ml release.ml sources.ml printer.ml evolution.ml debutil.ml debcudf.ml \
apt.ml edsp.ml

define PROJ_dose_debian
  SOURCES = $(call addmli,dose/deb,$(SRC_dose_debian))
  RESULT = dose_debian
  LIB_PACK_NAME = debian
  INCDIRS = extlib cudf re/lib
endef
export PROJ_dose_debian

SRC_dose_algo = defaultgraphs.ml diagnostic_int.ml diagnostic.ml		\
dominators.ml flatten.ml statistics.ml depsolver_int.ml depsolver.ml		\
strongconflicts_int.ml strongconflicts.ml strongdeps_int.ml strongdeps.ml

define PROJ_dose_algo
  SOURCES = $(call addmli,dose/algo,$(SRC_dose_algo))
  RESULT = dose_algo
  LIB_PACK_NAME = algo
  INCDIRS = extlib graph/src dose/common dose/deb cudf
endef
export PROJ_dose_algo

define PROJ_uutf
  SOURCES = $(call addmli,uutf/src,uutf.ml)
  RESULT = uutf
  LIB_PACK_NAME =
endef
export PROJ_uutf

define PROJ_jsonm
  SOURCES = $(call addmli,jsonm/src,jsonm.ml)
  RESULT = jsonm
  LIB_PACK_NAME =
  INCDIRS = uutf/src
endef
export PROJ_jsonm

# --

ifndef SUBROJS
  export SUBPROJS = $(LIB_EXTS)
endif


%:
	$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@

OCAMLMAKEFILE = ../OCamlMakefile
export OCAMLMAKEFILE
