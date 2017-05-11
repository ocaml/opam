-include ../Makefile.config

SRC_EXTS = cppo extlib re cmdliner graph cudf dose uutf jsonm opam_file_format

URL_cppo = https://github.com/mjambon/cppo/archive/v1.3.2.tar.gz
MD5_cppo = 133c9f8afadb6aa1c5ba0f5eb55c5648

URL_extlib = https://github.com/ygrek/ocaml-extlib/archive/1.7.0.tar.gz
MD5_extlib = b50b02d9e40d35cc20c82d9c881a1bf6 

URL_re = https://github.com/ocaml/ocaml-re/archive/ocaml-re-1.5.0.tar.gz
MD5_re = 1e9bff3279648881be31e5ff96219f0a

URL_cmdliner = http://erratique.ch/software/cmdliner/releases/cmdliner-0.9.8.tbz
MD5_cmdliner = fc67c937447cc223722f1419fa2189da

URL_graph = http://ocamlgraph.lri.fr/download/ocamlgraph-1.8.6.tar.gz
MD5_graph = afbc24f0e0eb72c2d3eda64b68513e73

URL_cudf = https://gforge.inria.fr/frs/download.php/33593/cudf-0.7.tar.gz
MD5_cudf = 2047222fcf78278c6a24ac619fc39abb

URL_dose = https://gforge.inria.fr/frs/download.php/file/35975/dose3-5.0.tar.gz
MD5_dose = 228c6a73a0759783fd01181047046610

URL_uutf = http://erratique.ch/software/uutf/releases/uutf-0.9.3.tbz
MD5_uutf = 708c0421e158b390c7cc341f37b40add

URL_jsonm = http://erratique.ch/software/jsonm/releases/jsonm-0.9.1.tbz
MD5_jsonm = 631a5dabdada83236c83056f60e42685

URL_opam_file_format = https://github.com/ocaml/opam-file-format/archive/2.0.0-beta3.tar.gz
MD5_opam_file_format = fb461d14a44aac3a43751aa936e79143

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

clone: $(SRC_EXTS:=.stamp) graph-workaround extlib-cppo dose-cppo
	@

archives: $(SRC_EXTS:=.download)
	@

define cache_url
https://opam.ocaml.org/2.0~dev/cache/md5/$(shell echo $(MD5_$(1)) | cut -c -2)/$(MD5_$(1))
endef

define get_from_cache
{ $(FETCH) $(call cache_url,$(1)) && \
  mv $(MD5_$(1)) $(notdir $(URL_$(1))) && \
  $(MD5CHECK) $(notdir $(URL_$(1))) $(MD5_$(1)); }
endef

%.download:
	[ -e  $(notdir $(URL_$*)) ] || \
	$(FETCH) $(URL_$*) && $(MD5CHECK) $(notdir $(URL_$*)) $(MD5_$*) || \
	$(call get_from_cache,$*)

%.stamp: %.download
	mkdir -p tmp
	cd tmp && $(if $(patsubst %.tar.gz,,$(URL_$*)),bunzip2,gunzip) -c ../$(notdir $(URL_$*)) | tar x
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

SRC_cppo = cppo_version.ml cppo_types.ml cppo_parser.mli cppo_parser.ml cppo_lexer.ml \
					 cppo_command.ml cppo_eval.ml cppo_main.ml

make-cppo:
	echo 'let cppo_version = "$(VERSION)"' > cppo/cppo_version.ml
	ocamllex cppo/cppo_lexer.mll
	ocamlyacc cppo/cppo_parser.mly
	ocamlopt -o cppo/cppo -I cppo -dtypes unix.cmxa str.cmxa $(foreach ml,$(SRC_cppo),cppo/$(ml))

ifeq ($(OCAML_4_02),true)
CPPOVARS=-D OCAML4 -D OCAML_4_02
else
CPPOVARS=-D OCAML4
endif

extlib-cppo: make-cppo
	cppo/cppo $(CPPOVARS) extlib/src/extBytes.mlpp -o extlib/src/extBytes.ml	
	cppo/cppo $(CPPOVARS) extlib/src/extHashtbl.mlpp -o extlib/src/extHashtbl.ml
	cppo/cppo $(CPPOVARS) extlib/src/extString.mlpp -o extlib/src/extString.ml
	cppo/cppo $(CPPOVARS) extlib/src/extBuffer.mlpp -o extlib/src/extBuffer.ml

dose-cppo:
	for f in `find dose -regex ".*.mli*"`; do\
		echo "(*pp cppo/cppo -V OCAML:$(OCAMLVERSION) -D 'OCAMLGRAPHVERSION 186' *)"|cat - $$f > /tmp/out && mv /tmp/out $$f;\
	done

clean:
	rm -rf lib
	$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBTARGET=cleanup

distclean:
	rm -rf cudf extlib re graph dose cmdliner uutf jsonm opam_file_format
	rm -f depends.ocp
	rm -f *.tar.gz *.tbz *.stamp
	rm -f *.cm* *.o *.a *.lib
	rm -rf lib

LIB_EXTS = extlib re cmdliner graph cudf dose_common dose_versioning dose_pef dose_opam dose_algo uutf jsonm opam_file_format

inst_objs = cp $(1)/*.cm*i lib

copy: build
	mkdir -p lib
	cp *.cma lib || true
	cp *.cmxa lib || true
	cp *.a lib || true
	cp *.lib lib || true
	$(call inst_objs,extlib/src)
	$(call inst_objs,re/lib)
	$(call inst_objs,cmdliner/src)
	$(call inst_objs,graph/src)
	$(call inst_objs,cudf)
	$(call inst_objs,dose/algo)
	$(call inst_objs,uutf/src)
	$(call inst_objs,jsonm/src)
	$(call inst_objs,opam_file_format/src)
	$(call inst_objs,.)

# --

addmli = $(foreach ml,$(2),$(wildcard $(addsuffix .mli,$(basename $(1)/$(ml)))) $(1)/$(ml))

SRC_extlib = extBytes.ml extBuffer.ml enum.ml extHashtbl.ml extList.ml extString.ml global.ml \
IO.ml option.ml pMap.ml refList.ml std.ml uChar.ml unzip.ml uTF8.ml optParse.ml	\
dynArray.ml dllist.ml bitSet.ml base64.ml extArray.ml extLib.ml

define PROJ_extlib
  SOURCES = $(call addmli,extlib/src,$(SRC_extlib))
  RESULT = extlib
  # Don't omit !! it leaks between subprojs...
  LIB_PACK_NAME =
endef
export PROJ_extlib

SRC_re = re_cset.ml re_automata.ml re.ml re_glob.ml re_perl.ml re_pcre.ml

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
cudf_822_parser.mly cudf_822_lexer.mll cudf_types_pp.ml \
cudf.ml cudf_checker.ml cudf_parser.mly cudf_printer.ml \
cudf_c.ml

define PROJ_cudf
  SOURCES = $(call addmli,cudf,$(SRC_cudf))
  RESULT = cudf
  LIB_PACK_NAME =
  INCDIRS = extlib/src
endef
export PROJ_cudf

SRC_dose_common = shell_lexer.mll util.ml url.ml input.ml\
									format822.ml format822_parser.mly format822_lexer.mll \
									criteria_parser.mly criteria_lexer.mll criteria_types.ml criteria.ml\
									edosSolver.ml cudfSolver.ml	cudfAdd.ml cudfDiff.ml

define PROJ_dose_common
  SOURCES = $(call addmli,dose/common,$(SRC_dose_common))
  RESULT = dose_common
  LIB_PACK_NAME = common
  INCDIRS = extlib/src cudf re/lib
endef
export PROJ_dose_common

SRC_dose_versioning = debian.ml semverNode.ml semver.ml utils.ml

define PROJ_dose_versioning
  SOURCES = $(call addmli,dose/versioning,$(SRC_dose_versioning))
  RESULT = dose_versioning
  LIB_PACK_NAME = versioning
  INCDIRS = extlib/src cudf re/lib
endef
export PROJ_dose_versioning

SRC_dose_pef = packages_types.ml packages_parser.mly packages_lexer.mll printer.ml packages.ml \
							 pefcudf.ml 

#SOURCES = $(SRC_dose_pef)

define PROJ_dose_pef
  SOURCES = $(call addmli,dose/pef,$(SRC_dose_pef))
  RESULT = dose_pef
  LIB_PACK_NAME = pef
  INCDIRS = extlib/src cudf re/lib
endef
export PROJ_dose_pef

SRC_dose_opam = opamcudf.ml packages.ml

define PROJ_dose_opam
  SOURCES = $(call addmli,dose/opam,$(SRC_dose_opam))
  RESULT = dose_opam
  LIB_PACK_NAME = opam
  INCDIRS = extlib/src cudf re/lib
endef
export PROJ_dose_opam

SRC_dose_algo = defaultgraphs.ml diagnostic.ml dominators.ml flatten.ml \
								statistics.ml depsolver_int.ml depsolver.ml strongconflicts_int.ml\
								strongconflicts.ml strongdeps.ml

define PROJ_dose_algo
  SOURCES = $(call addmli,dose/algo,$(SRC_dose_algo))
  RESULT = dose_algo
  LIB_PACK_NAME = algo
  INCDIRS = extlib/src graph/src cudf
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

SRC_opam_file_format = \
  opamParserTypes.mli \
  opamLexer.mli opamLexer.mll \
  opamBaseParser.mly \
	opamParser.mli opamParser.ml \
  opamPrinter.mli  opamPrinter.ml

define PROJ_opam_file_format
  SOURCES = $(call addmli,opam_file_format/src,$(SRC_opam_file_format))
  RESULT = opam-file-format
  LIB_PACK_NAME =
endef
export PROJ_opam_file_format

# --

ifndef SUBROJS
  export SUBPROJS = $(LIB_EXTS)
endif


%:
	$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@

OCAMLMAKEFILE = ../OCamlMakefile
export OCAMLMAKEFILE
