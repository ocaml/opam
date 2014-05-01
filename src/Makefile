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

OCAMLFLAGS = -g -w +a-4-9-32-41-44-45
OCAMLLDFLAGS = -g -w +a-4-9-32-41-44-45
ifeq ($OCAML_4,true)
  OCAMLFLAGS += -bin-annot
  OCAMLLDFLAGS += -bin-annot
endif
ifeq ($OCAML_4_01,true)
  OCAMLFLAGS += -short-paths
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
  LIBS = unix extlib re cmdliner graph cudf dose_common dose_debian dose_algo
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

SUBS = core solver repositories client
OPAMLIB = $(patsubst %,./opam-%,$(SUBS))
INCDIRS = $(EXT_INCDIRS) $(SUBS)

export INCDIRS

opam-lib.byte: $(OPAMLIB:=.cma)
	@

opam-lib.native: $(OPAMLIB:=.cmxa)
	@

opam-lib: opam-lib.byte $(if $(USE_BYTE),,opam-lib.native) opam-admin.top
	@

addmli = $(foreach ml,$(2),$(wildcard $(addsuffix .mli,$(basename $(1)/$(ml)))) $(1)/$(ml))

subtarget = SUBTARGET=$(if $(patsubst %.cma,,$@),native-code-library,byte-code-library)

opam-%.cma opam-%.cmxa:
	$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBPROJS=$* $(subtarget)

ALWAYS:
	@

# -- opam-core -- #
opam-core.cma opam-core.cmxa: core/opamScript.ml core/opamGitVersion.ml ALWAYS

GITREF := $(addsuffix ../.git/, $(shell cat ../.git/HEAD | cut -d ' ' -f 2))
GITDEP := ../.git/HEAD $(wildcard ${GITREF})

core/opamGitVersion.ml: ../shell/get-git-id.ml ${GITDEP}
	cd .. && ocaml shell/get-git-id.ml src/$@
	touch $@

core/opamScript.ml: ../shell core/opamVersion.ml
	ocaml ../shell/crunch.ml "complete"     < ../shell/opam_completion.sh > $@
	ocaml ../shell/crunch.ml "complete_zsh" < ../shell/opam_completion_zsh.sh >> $@
	ocaml ../shell/crunch.ml "switch_eval"  < ../shell/opam_switch_eval.sh >> $@

SRC_core = \
  opamJson.ml \
  opamLineLexer.mll \
  opamMisc.ml \
  opamScript.ml \
  opamGitVersion.ml \
  opamVersion.ml \
  opamGlobals.ml \
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
  opamTypes.ml \
  opamTypesBase.ml \
  opamPath.ml \
  opamParser.mly \
  opamLexer.mll \
  opamFormat.ml \
  opamFile.ml \
  opamActionGraph.ml \
  opamFilter.ml \
  opamRepository.ml

define PROJ_core
  SOURCES = $(call addmli,core,$(SRC_core))
  RESULT = opam-core
endef
export PROJ_core

# -- opam-solver -- #
opam-solver.cma: opam-core.cma
opam-solver.cmxa: opam-core.cmxa

SRC_solver = \
  opamCudf.ml \
  opamHeuristic.ml \
  opamSolver.ml

define PROJ_solver
  SOURCES = $(call addmli,solver,$(SRC_solver))
  RESULT = opam-solver
endef
export PROJ_solver

# -- opam-repositories -- #
opam-repositories.cma: opam-core.cma ALWAYS
opam-repositories.cmxa: opam-core.cmxa ALWAYS

SRC_repositories = \
  opamHTTP.ml \
  opamLocal.ml \
  opamVCS.ml \
  opamGit.ml \
  opamDarcs.ml \
  opamHg.ml

define PROJ_repositories
  SOURCES = $(call addmli,repositories,$(SRC_repositories))
  RESULT = opam-repositories
endef
export PROJ_repositories

# -- opam-client -- #
opam-client.cma: opam-core.cma opam-solver.cma opam-repositories.cma ALWAYS
opam-client.cmxa: opam-core.cmxa opam-solver.cmxa opam-repositories.cmxa ALWAYS

SRC_client = \
  opamState.ml \
  opamAction.ml \
  opamSolution.ml \
  opamSwitchCommand.ml \
  opamConfigCommand.ml \
  opamRepositoryCommand.ml \
  opamPinCommand.ml \
  opamClient.ml \
  opamArg.ml

define PROJ_client
  SOURCES = $(call addmli,client,$(SRC_client))
  RESULT = opam-client
endef
export PROJ_client

# -- opam binary -- #
opam: $(addsuffix $(LIBEXT),$(OPAMLIB)) ALWAYS
	$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBPROJS=opam SUBTARGET=$(BINTARGET)

define PROJ_opam
  SOURCES = client/opamMain.ml
  RESULT = opam
  LIBS = $(LIBS) $(OPAMLIB)
endef
export PROJ_opam

# -- opam utils -- #

SCRIPTS = opam-check opam-admin opam-installer opamlfind

$(SCRIPTS): opam ALWAYS
	$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBPROJS=$@ SUBTARGET=$(BINTARGET)

define PROJ_opam-check
  SOURCES = scripts/opam_check.ml
  RESULT = opam-check
  LIBS = $(LIBS) $(OPAMLIB)
endef
export PROJ_opam-check

SRC_opam-admin = \
  opam_mk_repo.ml \
  opam_repo_check.ml \
  opam_stats.ml \
  opam_depexts_change.ml \
  opam_libraries.ml \
  opam_admin.ml

define PROJ_opam-admin
  SOURCES = $(call addmli,scripts,$(SRC_opam-admin))
  RESULT = opam-admin
  LIBS = $(LIBS) $(OPAMLIB)
endef
export PROJ_opam-admin

define PROJ_opam-installer
  SOURCES = scripts/opam_installer.ml
  RESULT = opam-installer
  LIBS = $(LIBS) $(OPAMLIB)
endef
export PROJ_opam-installer

define PROJ_opamlfind
  SOURCES = scripts/opamlfind.ml
  RESULT = opamlfind
  LIBS = $(LIBS) $(OPAMLIB)
endef
export PROJ_opamlfind

opam-admin.top: opam-lib.byte ALWAYS
	$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBPROJS=opam-admin-top SUBTARGET=top

define PROJ_opam-admin-top
  SOURCES = scripts/opam_admin_top.ml
  RESULT = opam-admin
  LIBS = $(LIBS) $(OPAMLIB)
endef
export PROJ_opam-admin-top

# --

ifndef SUBPROJS
  export SUBPROJS = $(SUBS) opam opam-check opam-admin opam-installer opamlfind opam-admin-top
endif

OCAMLMAKEFILE = ../OCamlMakefile
export OCAMLMAKEFILE

clean-aux:
	rm -f $(OPAMLIB:=.cma) $(OPAMLIB:=.cmxa) $(OPAMLIB:=.a) $(SCRIPTS) opam-admin.top opam_admin_top.*

clean: clean-aux

%:
	$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@
