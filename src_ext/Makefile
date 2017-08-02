ifneq ($(filter-out archives cache-archives,$(MAKECMDGOALS)),)
-include ../Makefile.config
endif

SRC_EXTS = cppo extlib re cmdliner graph cudf dose opam_file_format

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

URL_opam_file_format = https://github.com/ocaml/opam-file-format/archive/2.0.0-beta3.tar.gz
MD5_opam_file_format = fb461d14a44aac3a43751aa936e79143

ifndef FETCH
ifneq ($(shell command -v curl 2>/dev/null),)
else
FETCH=wget
endif
endif

ARCHIVES = $(foreach lib,$(SRC_EXTS),$(notdir $(URL_$(lib))))
lib_of = $(foreach lib,$(SRC_EXTS),$(if $(findstring $(1),$(URL_$(lib))),$(lib),,))

ifdef OCAML
# Portable md5check
MD5CHECK = $(OCAML) ../shell/md5check.ml $(1) $(2)
else
MD5CHECK = test "`md5sum $(1) | sed -e 's/^[^a-f0-9]*\([a-f0-9]*\).*/\1/'`" = "$(2)" || (rm $(1) && false)
endif

lib-ext: clone build copy
	@

ifeq ($(OCAMLC),no)
SUBTARGET=ncl
else
ifeq ($(OCAMLOPT),no)
SUBTARGET=bcl
else
SUBTARGET="bcl ncl"
endif
endif

build: clone
	$(MAKE) $(SUBPROJS:%=proj_%)

all: bcl ncl

clone: $(SRC_EXTS:=.stamp) graph-workaround.stamp extlib-cppo.stamp dose-cppo.stamp
	@

.PHONY: archives
archives: $(SRC_EXTS:=.download)
	@

cache-archives: $(SRC_EXTS:=.cache)
	@

%.cache:
	@mkdir -p archives
	@[ -e archives/$(notdir $(URL_$*)) ] || \
		($(FETCH) $(URL_$*) && mv $(notdir $(URL_$*)) archives/)

define cache_url
https://opam.ocaml.org/2.0~dev/cache/md5/$(shell echo $(MD5_$(1)) | cut -c -2)/$(MD5_$(1))
endef

define get_from_cache
{ $(FETCH) $(call cache_url,$(1)) && \
  mv $(MD5_$(1)) $(notdir $(URL_$(1))) && \
  $(call MD5CHECK,$(notdir $(URL_$(1))),$(MD5_$(1))); }
endef

%.download:
	[ -e  $(notdir $(URL_$*)) ] || \
    cp archives/$(notdir $(URL_$*)) . 2>/dev/null || \
	$(FETCH) $(URL_$*) && $(call MD5CHECK,$(notdir $(URL_$*)),$(MD5_$*)) || \
	$(call get_from_cache,$*)
	@touch $@

%.stamp: %.download
	mkdir -p tmp-$*
	cd tmp-$* && $(if $(patsubst %.tar.gz,,$(URL_$*)),bunzip2,gunzip) -c ../$(notdir $(URL_$*)) | tar xf -
	rm -rf $*
	@for ii in tmp-$*/*; do if [ -d $${ii} ]; then mv $${ii} $*; fi; done; \
	rm -rf tmp-$*
	@if [ -d patches/$* ]; then \
          cd $* && \
	  for p in ../patches/$*/*.patch; do \
	    patch -p1 < $$p; \
	  done; \
        fi
	@touch $@

# OCamlMakefile doesn't include stand-alone mlis in the packs...
graph-workaround.stamp: graph.stamp
	cp graph/src/sig.mli graph/src/sig.ml
	cp graph/src/sig_pack.mli graph/src/sig_pack.ml
	cp graph/src/dot_ast.mli graph/src/dot_ast.ml
	@touch $@

SRC_cppo = cppo_version.ml cppo_types.ml cppo_parser.mli cppo_parser.ml cppo_lexer.ml \
					 cppo_command.ml cppo_eval.ml cppo_main.ml

cppo/cppo$(EXE): cppo.stamp
	echo 'let cppo_version = "$(VERSION)"' > cppo/cppo_version.ml
	ocamllex cppo/cppo_lexer.mll
	ocamlyacc cppo/cppo_parser.mly
	ocamlopt -o cppo/cppo$(EXE) -I cppo -dtypes unix.cmxa str.cmxa $(foreach ml,$(SRC_cppo),cppo/$(ml))

ifeq ($(OCAML_4_02),true)
CPPOVARS=-D OCAML4 -D OCAML_4_02
else
CPPOVARS=-D OCAML4
endif

extlib-cppo.stamp: cppo/cppo$(EXE) extlib.stamp
	cppo/cppo$(EXE) $(CPPOVARS) extlib/src/extBytes.mlpp -o extlib/src/extBytes.ml
	cppo/cppo$(EXE) $(CPPOVARS) extlib/src/extHashtbl.mlpp -o extlib/src/extHashtbl.ml
	cppo/cppo$(EXE) $(CPPOVARS) extlib/src/extString.mlpp -o extlib/src/extString.ml
	cppo/cppo$(EXE) $(CPPOVARS) extlib/src/extBuffer.mlpp -o extlib/src/extBuffer.ml
	@touch $@

dose-cppo.stamp: dose.stamp
	for f in `find dose -regex ".*.mli*"`; do\
		T=`mktemp`; \
		echo "(*pp cppo$(if $(filter 1,$(WIN32)),\,/)cppo$(EXE) -V OCAML:$(OCAMLVERSION) *)" > $$T; \
		if fgrep -q OCAMLGRAPHVERSION $$f ; then \
		  echo "#define OCAMLGRAPHVERSION 186" >> $$T ; \
		fi ; \
		cat $$f >> $$T && mv $$T $$f;\
	done
	@touch $@

clean:
	rm -rf lib
	$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBTARGET=cleanup

distclean:
	rm -rf cudf extlib re graph dose cmdliner opam_file_format ._ncdi ._bcdi ._d
	rm -f depends.ocp
	rm -f *.tar.gz *.tbz *.stamp *.download
	rm -f *.cm* *.o *.a *.lib *.obj
	rm -rf lib
	[ -d archives ] && ([ "$$(find archives -maxdepth 0 -type d -empty)" != "" ] && rmdir archives || echo "WARNING! $$(pwd)/archives/ not empty so left") || true

LIB_EXTS = extlib re cmdliner graph cudf dose_common dose_versioning dose_pef dose_opam dose_algo \
           opam_file_format

proj_cudf: proj_extlib

proj_dose_common: proj_extlib proj_re proj_cudf

proj_dose_versioning: proj_extlib proj_dose_common proj_re proj_cudf

proj_dose_pef: proj_extlib proj_dose_common proj_re proj_cudf

proj_dose_opam: proj_extlib proj_dose_pef proj_re proj_cudf

proj_dose_algo: proj_extlib proj_dose_common proj_graph proj_cudf

# --

addmli = $(foreach ml,$(2),$(wildcard $(addsuffix .mli,$(basename $(1)/$(ml)))) $(1)/$(ml))

define MAKE_LIB
define PROJ_$1
  SRC_DIR = $(lastword $2)
  SOURCES = $(foreach DIR,$(filter-out $(lastword $2),$2),$(call addmli,$(DIR),$(SRC_$1_$(notdir $(DIR))))) \
            $(call addmli,$(lastword $2),$(SRC_$1))
  RESULT = $(if $4,$4,$1)
  LIB_PACK_NAME = $(filter-out @,$(PACK_NAME))
  INCDIRS = $(foreach DEP,$3,$(eval $(PROJ_$(DEP)))$(SRC_DIR))
endef
export PROJ_$1
endef
PROCESS_LIB=$(eval $(call MAKE_LIB,$1,$(if $2,$(foreach DIR,$2,$1/$(DIR)),$1),$3,$4))
BASIC_LIB=$(foreach PACK_NAME,@,$(call PROCESS_LIB,$1,$2,$3,$4))
PACK_LIB=$(foreach PACK_NAME,$1,$(call PROCESS_LIB,$1,$2,$3,$4))
SUB_PACK_LIB=$(foreach PACK_NAME,$2,$(eval $(call MAKE_LIB,$1_$2,$1/$2,$3,$4)))

SRC_extlib = extBytes.ml extBuffer.ml enum.ml extHashtbl.ml extList.ml extString.ml global.ml \
IO.ml option.ml pMap.ml refList.ml std.ml uChar.ml unzip.ml uTF8.ml optParse.ml	\
dynArray.ml dllist.ml bitSet.ml base64.ml extArray.ml extLib.ml
$(call BASIC_LIB,extlib,src)

SRC_re = re_cset.ml re_automata.ml re.ml re_glob.ml re_perl.ml re_pcre.ml
$(call BASIC_LIB,re,lib)

SRC_cmdliner = cmdliner.ml
$(call BASIC_LIB,cmdliner,src)

SRC_graph_lib = bitv.ml heap.ml unionfind.ml
SRC_graph = sig_pack.ml sig.ml dot_ast.ml builder.ml classic.ml contraction.ml	\
delaunay.ml dominator.ml dot_parser.mly fixpoint.ml gmap.ml gml.mll graphml.ml	\
graphviz.ml leaderlist.ml oper.ml rand.ml strat.ml traverse.ml util.ml		\
version.ml kruskal.ml flow.ml dot_lexer.mll components.ml coloring.ml blocks.ml	\
dot.ml imperative.ml merge.ml minsep.ml persistent.ml nonnegative.ml path.ml	\
topological.ml pack.ml mcs_m.ml cliquetree.ml md.ml
$(call PACK_LIB,graph,lib src)

SRC_cudf = cudf_types.ml cudf_type_parser.mly cudf_type_lexer.mll cudf_conf.ml	\
cudf_822_parser.mly cudf_822_lexer.mll cudf_types_pp.ml \
cudf.ml cudf_checker.ml cudf_parser.mly cudf_printer.ml \
cudf_c.ml
$(call BASIC_LIB,cudf,,extlib)

SRC_dose_common = shell_lexer.mll util.ml url.ml input.ml\
									format822.ml format822_parser.mly format822_lexer.mll \
									criteria_parser.mly criteria_lexer.mll criteria_types.ml criteria.ml\
									edosSolver.ml cudfSolver.ml	cudfAdd.ml cudfDiff.ml
$(call SUB_PACK_LIB,dose,common,extlib cudf re)

SRC_dose_versioning = debian.ml semverNode.ml semver.ml utils.ml
$(call SUB_PACK_LIB,dose,versioning,extlib cudf re)

SRC_dose_pef = packages_types.ml packages_parser.mly packages_lexer.mll printer.ml packages.ml \
							 pefcudf.ml
$(call SUB_PACK_LIB,dose,pef,extlib cudf re)

SRC_dose_opam = opamcudf.ml packages.ml
$(call SUB_PACK_LIB,dose,opam,extlib cudf re)

SRC_dose_algo = defaultgraphs.ml diagnostic.ml dominators.ml flatten.ml \
								statistics.ml depsolver_int.ml depsolver.ml strongconflicts_int.ml\
								strongconflicts.ml strongdeps.ml
$(call SUB_PACK_LIB,dose,algo,extlib graph cudf)

SRC_opam_file_format = \
  opamParserTypes.mli \
  opamLexer.mli opamLexer.mll \
  opamBaseParser.mly \
	opamParser.mli opamParser.ml \
  opamPrinter.mli  opamPrinter.ml
$(call BASIC_LIB,opam_file_format,src,,opam-file-format)

# --

define INSTALL_OBJECTS
	cp $(eval $(call PROJ_$(LIB)))$(if $(LIB_PACK_NAME),$(LIB_PACK_NAME).cm*i $(LIB_PACK_NAME).cmx,$(SRC_DIR)/*.cm*i $(SRC_DIR)/*.cmx) lib 2>/dev/null || true

endef

copy: build
	mkdir -p lib
	cp *.cma *.cmxa *$(shell ocamlc -config | grep "^ext_lib:" | tr -d '\r' | sed -e "s/.*: //") lib 2>/dev/null || true
	$(foreach LIB,$(LIB_EXTS),$(INSTALL_OBJECTS))

# --

ifndef SUBROJS
  export SUBPROJS = $(LIB_EXTS)
endif

proj_%:
	+$(MAKE) -f $(OCAMLMAKEFILE) subproj_$* SUBTARGET=$(SUBTARGET) make_deps=yes

OCAMLMAKEFILE = ../OCamlMakefile
export OCAMLMAKEFILE
