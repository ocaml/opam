-include ../Makefile.config

.NOTPARALLEL:

ifndef version
  $(error Please run ./configure)
endif

all:
	$(MAKE) opam-lib.byte
	$(MAKE) opam-lib
	$(MAKE) opam
	$(MAKE) opam-check
	$(MAKE) opam-installer
	$(MAKE) opam-admin.top

# --

OCAMLFLAGS = -g -w +a-4-44-48 -bin-annot -short-paths
OCAMLLDFLAGS = -g -w +a-4-44-48 -bin-annot
ifeq ($(OCAML_4_02),true)
  OCAMLFLAGS += -safe-string
  OCAMLLDFLAGS += -safe-string
endif

export OCAMLFLAGS OCAMLLDFLAGS

BINTARGET = $(if $(USE_BYTE),byte-code,native-code)

# --

ifndef HAS_LIBEXT
  HAS_LIBEXT := $(wildcard ../src_ext/lib)
endif

ifneq ($(HAS_LIBEXT),)
  EXT_INCDIRS = ../src_ext/lib
  LIBS = unix bigarray extlib re cmdliner graph cudf dose_common dose_algo uutf jsonm opam-file-format
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

SUBS = core format solver repository state client
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
opam-core.cma opam-core.cmxa: state/opamScript.ml core/opamCompat.ml core/opamCompat.mli ALWAYS

state/opamScript.ml: ../shell core/opamVersion.ml
	ocaml ../shell/crunch.ml "complete"     < ../shell/opam_completion.sh > $@
	ocaml ../shell/crunch.ml "complete_zsh" < ../shell/opam_completion_zsh.sh >> $@
	ocaml ../shell/crunch.ml "prompt"       < ../shell/opam_prompt.sh >> $@

ifeq ($(OCAML_4_03),true)
  COMPATV = 4.03
else
  ifeq ($(OCAML_4_02),true)
    COMPATV = 4.02
  else
    COMPATV = 4.01
  endif
endif

core/opamCompat.%: core/opamCompat.%.$(COMPATV)
	cp -f $< $@

SRC_core = \
  opamCompat.ml \
  opamJson.ml \
  opamSHA.ml \
  opamCoreConfig.ml \
  opamStd.ml \
  opamConsole.ml \
  opamVersionCompare.ml \
  opamVersion.ml \
  opamProcess.ml \
  opamSystem.ml \
  opamParallel.ml \
  opamHash.ml \
  opamFilename.ml \
  opamDirTrack.ml \
  opamUrl.ml

define PROJ_core
  SOURCES = $(call addmli,core,$(SRC_core))
  RESULT = opam-core
endef
export PROJ_core


# -- opam-format -- #

opam-repository.cma: opam-core.cma opam-format.cma ALWAYS
opam-repository.cmxa: opam-core.cmxa opam-format.cmxa ALWAYS

SRC_format = \
  opamFormatConfig.ml \
  opamLineLexer.mll \
  opamRepositoryName.ml \
  opamSwitch.ml \
  opamPackage.ml \
  opamVariable.ml \
  opamFormula.ml \
  opamTypes.mli \
  opamTypesBase.ml \
  opamFilter.ml \
  opamPp.ml \
  opamFormat.ml \
  opamFile.ml

define PROJ_format
  SOURCES = $(call addmli,format,$(SRC_format))
  RESULT = opam-format
endef
export PROJ_format

# -- opam-solver -- #
opam-solver.cma: opam-core.cma opam-format.cma ALWAYS
opam-solver.cmxa: opam-core.cmxa opam-format.cmxa ALWAYS

SRC_solver = \
  opamSolverConfig.ml \
  opamActionGraph.ml \
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

# -- opam-state -- #
opam-state.cma: opam-core.cma opam-format.cma opam-repository.cma ALWAYS
opam-state.cmxa: opam-core.cmxa opam-format.cmxa opam-repository.cmxa ALWAYS

SRC_state = \
  opamPath.ml \
  opamStateConfig.ml \
  opamScript.ml \
  opamStateTypes.mli \
  opamPackageVar.ml \
  opamFileTools.ml \
  opamEnv.ml \
  opamFormatUpgrade.ml \
  opamGlobalState.ml \
  opamRepositoryState.ml \
  opamPinned.ml \
  opamSwitchState.ml \
  opamSwitchAction.ml \
  opamUpdate.ml

define PROJ_state
  SOURCES = $(call addmli,state,$(SRC_state))
  RESULT = opam-state
endef
export PROJ_state

# -- opam-client -- #
opam-client.cma: client/opamGitVersion.ml opam-core.cma opam-format.cma opam-solver.cma opam-repository.cma opam-state.cma ALWAYS
opam-client.cmxa: client/opamGitVersion.ml opam-core.cmxa opam-format.cmxa opam-solver.cmxa opam-repository.cmxa opam-state.cmxa ALWAYS

SRC_client = \
  opamClientConfig.ml \
  opamAction.ml \
  opamSolution.ml \
  opamSwitchCommand.ml \
  opamConfigCommand.ml \
  opamAdminRepoUpgrade.ml \
  opamRepositoryCommand.ml \
  opamPinCommand.ml \
  opamListCommand.ml \
  opamInitDefaults.ml \
  opamClient.ml \
  opamAuxCommands.ml \
  opamGitVersion.ml \
  opamArg.ml \
  opamAdminCommand.ml \
  opamCommands.ml

define PROJ_client
  SOURCES = $(call addmli,client,$(SRC_client))
  RESULT = opam-client
endef
export PROJ_client

# -- opam binary -- #

client/opamGitVersion.ml: ALWAYS
	cd .. && ocaml shell/get-git-id.ml src/$@
	touch $@

opam: $(addsuffix $(LIBEXT),$(OPAMLIB)) ALWAYS
	$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBPROJS=opam SUBTARGET=$(BINTARGET)

define PROJ_opam
  SOURCES = $(call addmli,client,opamMain.ml)
  RESULT = opam
  LIBS = $(LIBS) $(OPAMLIB)
endef
export PROJ_opam

# -- opam utils -- #

TOOLS = opam-check opam-installer

$(TOOLS): opam ALWAYS
	$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBPROJS=$@ SUBTARGET=$(BINTARGET)

define PROJ_opam-check
  SOURCES = tools/opam_check.ml
  RESULT = opam-check
  LIBS = $(LIBS) $(OPAMLIB)
endef
export PROJ_opam-check

define PROJ_opam-installer
  SOURCES = tools/opam_installer.ml
  RESULT = opam-installer
  LIBS = $(LIBS) opam-core opam-format opam-state
endef
export PROJ_opam-installer

opam-admin.top: opam-lib.byte ALWAYS
	$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBPROJS=opam-admin-top SUBTARGET=top

SRC_opam_admin_top = opam_admin_top.ml

define PROJ_opam-admin-top
  SOURCES = $(call addmli,tools,$(SRC_opam_admin_top))
  RESULT = opam-admin
  LIBS = $(LIBS) $(OPAMLIB)
endef
export PROJ_opam-admin-top

# --

# Looks for artifacts in sourcedir, _obuild, sourcedir/_obuild
# The artifacts must be built first. Names relative from project root.
define lib_inst_files
  $(addprefix src/,\
    $(wildcard /opam-$(1).cma opam-$(1).cmxa opam-$(1).a) \
    $(foreach f,$(basename $(SRC_$(1))),\
      $(wildcard \
        $(addprefix $(1)/$(f),\
          .mli .cmi .cmo .cmx .cmxs .cmti)))) \
  $(patsubst ../%,%,\
    $(wildcard \
      $(addprefix ../_obuild/opam-$(1)/*,\
        .cma .cmxa .a .cmi .cmo .cmx .cmxs .cmti)) \
    $(wildcard \
      $(addprefix $(1)/_obuild/opam-$(1)/*,\
        .cma .cmxa .a .cmi .cmo .cmx .cmxs .cmti)))
endef

../opam-%.install: ALWAYS
	@echo 'lib: [' >$@
	@echo '  "src/$*.META" {"META"}' >>$@
	@{ $(patsubst %,echo '  "%"';,$(call lib_inst_files,$*)) } >>$@
	@echo ']' >>$@

../opam-admin.install: ALWAYS
	@echo 'lib: [' >$@
	@echo '  "src/$*.META" {"META"}' >>$@
	@{ $(patsubst %,echo '  "%"';,$(call lib_inst_files,tools)) } >>$@
	@echo ']' >>$@
	@echo 'bin: [' >>$@
	@echo '  "src/opam-admin.top"' >>$@
	@echo ']' >>$@

# Older, all-inclusive opam-lib with ocamlfind sub-packages
../opam-lib.install: ALWAYS
	@echo 'lib: [' >$@
	@echo '  "META"' >>$@
	@{ $(patsubst %,echo '  "%"';,\
	    $(foreach lib,$(SUBS),$(call lib_inst_files,$(lib))))\
	} >>$@
	@{ $(patsubst %,echo '  "%"';,\
	    $(wildcard \
	      $(addprefix tools/opam_admin_top.,mli cmi cmx cmxs cmti)))\
	} >>$@
	@echo ']' >>$@
	@echo 'bin: [' >>$@
	@echo '  "src/opam-admin.top"' >>$@
	@echo ']' >>$@
	@echo 'doc: [' >>$@
	@$(foreach x,$(wildcard ../doc/html/*.html ../doc/html/ocamldoc/*.html),\
	  echo '  "$(patsubst ../%,%,$x)" {"$(patsubst ../doc/html/%,%,$x)"}' >>$@;)
	@echo ']' >>$@

.PHONY: dotinstalls
dotinstalls: $(patsubst %,../opam-%.install,$(SUBS) admin)

addmli = $(foreach ml,$(2),$(wildcard $(addsuffix .mli,$(basename $(1)/$(ml)))) $(1)/$(ml))

ifndef SUBPROJS
  export SUBPROJS = $(SUBS) opam opam-check opam-installer opam-admin-top
endif

OCAMLMAKEFILE = ../OCamlMakefile
export OCAMLMAKEFILE

clean-aux:
	rm -f $(foreach e,a cma cmxa,$(wildcard *.$e))
	rm -f client/opamGitVersion.ml state/opamScript.ml core/opamCompat.ml core/opamCompat.mli ../opam-lib.install
	rm -f */*Parser.ml */*Parser.mli */*Lexer.ml
	rm -f $(foreach e,o cmo cmx cmxs cmi cmt cmti,$(wildcard */*.$e))

clean: clean-aux

%:
	$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@
