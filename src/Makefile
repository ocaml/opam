-include ../Makefile.config

ifndef version
  $(error Please run ./configure)
endif

all:
	$(MAKE) opam-lib
	$(MAKE) opam
	$(MAKE) opam-check
	$(MAKE) opam-admin
	$(MAKE) opam-installer
	$(MAKE) opamlfind
	$(MAKE) opam-admin.top

# --

OCAMLFLAGS = -g -w +a-4-44-48
OCAMLLDFLAGS = -g -w +a-4-44-48
ifeq ($(OCAML_4),true)
  OCAMLFLAGS += -bin-annot
  OCAMLLDFLAGS += -bin-annot
endif
ifeq ($(OCAML_4_01),true)
  OCAMLFLAGS += -short-paths
endif
ifeq ($(OCAML_4_02),true)
  OCAMLFLAGS += -safe-string
  OCAMLLDFLAGS += -safe-string
endif

export OCAMLFLAGS OCAMLLDFLAGS

USE_BYTE ?=
LIBEXT = $(if $(USE_BYTE),.cma,.cmxa)
BINTARGET = $(if $(USE_BYTE),byte-code,native-code)

# --

ifndef HAS_LIBEXT
  HAS_LIBEXT := $(wildcard ../src_ext/lib)
endif

ifneq ($(HAS_LIBEXT),)
  EXT_INCDIRS = ../src_ext/lib
  LIBS = unix extlib re cmdliner graph cudf dose_common dose_debian dose_algo uutf jsonm
else
  ifeq ($(HAS_PACKAGES),)
    $(error Dependencies missing. Either run 'make lib-ext' or install them and re-run './configure')
  endif
  export PACKS
  # Reset command name for ocamlfind
  OCAMLC = ocamlc
  OCAMLOPT = ocamlopt
  OCAMLDEP = ocamldep
  OCAMLLEX = ocamllex
  OCAMLYACC = ocamlyacc
  OCAMLMKLIB = ocamlmklib
  OCAMLDOC = ocamldoc
  export OCAMLC OCAMLOPT OCAMLDEP OCAMLLEX OCAMLYACC OCAMLMKLIB OCAMLDOC
endif


# --

SUBS = core solver repository client
OPAMLIB = $(patsubst %,./opam-%,$(SUBS))
INCDIRS = $(EXT_INCDIRS) $(SUBS)

export INCDIRS

opam-lib.byte:
	$(MAKE) $(OPAMLIB:=.cma)

opam-lib.native:
	$(MAKE) $(OPAMLIB:=.cmxa)

opam-lib: opam-lib.byte $(if $(USE_BYTE),,opam-lib.native) opam-admin.top
	@

addmli = $(foreach ml,$(2),$(wildcard $(addsuffix .mli,$(basename $(1)/$(ml)))) $(1)/$(ml))

subtarget = SUBTARGET=$(if $(patsubst %.cma,,$@),native-code-library,byte-code-library)

opam-%.cma opam-%.cmxa:
	$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBPROJS=$* $(subtarget)

ALWAYS:
	@

# -- opam-core -- #
opam-core.cma opam-core.cmxa: core/opamScript.ml core/opamCompat.ml core/opamCompat.mli ALWAYS

core/opamScript.ml: ../shell core/opamVersion.ml
	ocaml ../shell/crunch.ml "complete"     < ../shell/opam_completion.sh > $@
	ocaml ../shell/crunch.ml "complete_zsh" < ../shell/opam_completion_zsh.sh >> $@
	ocaml ../shell/crunch.ml "switch_eval"  < ../shell/opam_switch_eval.sh >> $@

ifeq ($(OCAML_4_02),true)
  COMPATV = 4.02
else
  COMPATV = 4.01
endif

core/opamCompat.%: core/opamCompat.%.$(COMPATV)
	cp -f $< $@

SRC_core = \
  opamCompat.ml \
  opamJson.ml \
  opamCoreConfig.ml \
  opamLineLexer.mll \
  opamStd.ml \
  opamConsole.ml \
  opamScript.ml \
  opamVersionCompare.ml \
  opamVersion.ml \
  opamProcess.ml \
  opamRepositoryName.ml \
  opamSwitch.ml \
  opamSystem.ml \
  opamParallel.ml \
  opamFilename.ml \
  opamPackage.ml \
  opamVariable.ml \
  opamFormula.ml \
  opamCompiler.ml \
  opamTypes.mli \
  opamTypesBase.ml \
  opamParser.mly \
  opamLexer.mll \
  opamFormat.ml \
  opamFilter.ml \
  opamFile.ml \
  opamActionGraph.ml

define PROJ_core
  SOURCES = $(call addmli,core,$(SRC_core))
  RESULT = opam-core
endef
export PROJ_core

# -- opam-solver -- #
opam-solver.cma: opam-core.cma
opam-solver.cmxa: opam-core.cmxa

SRC_solver = \
  opamSolverConfig.ml \
  opamCudf.ml \
  opamHeuristic.ml \
  opamSolver.ml

define PROJ_solver
  SOURCES = $(call addmli,solver,$(SRC_solver))
  RESULT = opam-solver
endef
export PROJ_solver

# -- opam-repository -- #
opam-repository.cma: opam-core.cma ALWAYS
opam-repository.cmxa: opam-core.cmxa ALWAYS

SRC_repository = \
  opamRepositoryConfig.ml \
  opamDownload.ml \
  opamRepositoryBackend.ml \
  opamRepositoryPath.ml \
  opamHTTP.ml \
  opamLocal.ml \
  opamVCS.ml \
  opamGit.ml \
  opamDarcs.ml \
  opamHg.ml \
  opamRepository.ml

define PROJ_repository
  SOURCES = $(call addmli,repository,$(SRC_repository))
  RESULT = opam-repository
endef
export PROJ_repository

# -- opam-client -- #
opam-client.cma: opam-core.cma opam-solver.cma opam-repository.cma ALWAYS
opam-client.cmxa: opam-core.cmxa opam-solver.cmxa opam-repository.cmxa ALWAYS

SRC_client = \
  opamPath.ml \
  opamClientConfig.ml \
  opamClientGlobals.ml \
  opamState.ml \
  opamAction.ml \
  opamSolution.ml \
  opamSwitchCommand.ml \
  opamConfigCommand.ml \
  opamRepositoryCommand.ml \
  opamPinCommand.ml \
  opamClient.ml

define PROJ_client
  SOURCES = $(call addmli,client,$(SRC_client))
  RESULT = opam-client
endef
export PROJ_client

# -- opam binary -- #

client/opamGitVersion.ml: ALWAYS
	cd .. && ocaml shell/get-git-id.ml src/$@
	touch $@

opam: $(addsuffix $(LIBEXT),$(OPAMLIB)) client/opamGitVersion.ml ALWAYS
	$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBPROJS=opam SUBTARGET=$(BINTARGET)

define PROJ_opam
  SOURCES = $(call addmli,client,opamGitVersion.ml opamArg.ml opamMain.ml)
  RESULT = opam
  LIBS = $(LIBS) $(OPAMLIB)
endef
export PROJ_opam

# -- opam utils -- #

TOOLS = opam-check opam-admin opam-installer opamlfind

$(TOOLS): opam ALWAYS
	$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBPROJS=$@ SUBTARGET=$(BINTARGET)

define PROJ_opam-check
  SOURCES = tools/opam_check.ml
  RESULT = opam-check
  LIBS = $(LIBS) $(OPAMLIB)
endef
export PROJ_opam-check

SRC_opam-admin = \
  opam_mk_repo.ml \
  opam_repo_check.ml \
  opam_stats.ml \
  opam_depexts_change.ml \
  opam_rename.ml \
  opam_admin.ml

# opam_findlib.ml

define PROJ_opam-admin
  SOURCES = $(call addmli,tools,$(SRC_opam-admin))
  RESULT = opam-admin
  LIBS = $(LIBS) $(OPAMLIB)
endef
export PROJ_opam-admin

define PROJ_opam-installer
  SOURCES = tools/opam_installer.ml
  RESULT = opam-installer
  LIBS = $(LIBS) $(OPAMLIB)
endef
export PROJ_opam-installer

define PROJ_opamlfind
  SOURCES = tools/opamlfind.ml
  RESULT = opamlfind
  LIBS = $(LIBS) $(OPAMLIB)
endef
export PROJ_opamlfind

opam-admin.top: opam-lib.byte ALWAYS
	$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBPROJS=opam-admin-top SUBTARGET=top

define PROJ_opam-admin-top
  SOURCES = tools/opam_admin_top.ml
  RESULT = opam-admin
  LIBS = $(LIBS) $(OPAMLIB)
endef
export PROJ_opam-admin-top

# --

define lib_inst_files =
  $(wildcard opam-$(1).cma opam-$(1).cmxa opam-$(1).a) \
  $(foreach f,$(basename $(SRC_$(1))),\
    $(wildcard $(addprefix $(1)/$(f),.mli .cmi .cmx .cmxs .cmti)))
endef

../opam-lib.install: $(OPAMLIB:=.cma)
	@echo 'lib: [' >$@
	@echo '  "META"' >>$@
	@{ $(patsubst %,echo '  "src/'%'"';,\
	    $(foreach lib,$(SUBS),$(call lib_inst_files,$(lib))))\
	} >>$@
	@echo ']' >>$@
	@echo 'bin: [' >>$@
	@echo '  "src/opam-admin.top"' >>$@
	@echo ']' >>$@


addmli = $(foreach ml,$(2),$(wildcard $(addsuffix .mli,$(basename $(1)/$(ml)))) $(1)/$(ml))

ifndef SUBPROJS
  export SUBPROJS = $(SUBS) opam opam-check opam-admin opam-installer opamlfind opam-admin-top
endif

OCAMLMAKEFILE = ../OCamlMakefile
export OCAMLMAKEFILE

clean-aux:
	rm -f $(OPAMLIB:=.cma) $(OPAMLIB:=.cmxa) $(OPAMLIB:=.a) $(TOOLS) opam-admin.top opam_admin_top.*
	rm -f client/opamGitVersion.ml core/opamScript.ml core/opamCompat.ml core/opamCompat.mli

clean: clean-aux

%:
	$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@
