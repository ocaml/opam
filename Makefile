ifeq ($(filter distclean clean,$(MAKECMDGOALS)),)
-include Makefile.config
endif

all: opam opam-installer
	@

admin:
	$(DUNE) build $(DUNE_PROFILE_ARG) --root . $(DUNE_ARGS) opam-admin.install

# $(call CYGPATH,file) will convert file to a Windows path if opam is being
# compiled for native Windows _and_ cygpath is available in PATH. The check
# on PATH is done once only.
ifeq ($(WIN32),1)
  CYGPATH = \
    $(eval CYGPATH = $(if $(shell command -v cygpath 2>/dev/null),\
                          $$(shell cygpath -w '$$(1)'),\
                          $$(1)))$(CYGPATH)
else
  CYGPATH = $(1)
endif

ifeq ($(DUNE),)
  DUNE_EXE = src_ext/dune-local/dune.exe
  ifeq ($(shell command -v cygpath 2>/dev/null),)
    DUNE := $(DUNE_EXE)
  else
    DUNE := $(shell echo "$(DUNE_EXE)" | cygpath -f - -a)
  endif
else
  DUNE_EXE=
endif

OPAMINSTALLER = ./opam-installer$(EXE)

ALWAYS:
	@

DUNE_DEP = $(DUNE_EXE)
JBUILDER_ARGS ?=
DUNE_ARGS ?= $(JBUILDER_ARGS)
DUNE_PROFILE ?= release

ifeq ($(DUNE_PROFILE_ARG),release)
  # TODO Replace with --release when we require dune >= 2.5
  DUNE_PROFILE_ARG = --profile=release
else
  DUNE_PROFILE_ARG = --profile=$(DUNE_PROFILE)
endif

src_ext/dune-local/dune.exe: src_ext/dune-local.stamp $(DUNE_SECONDARY)
ifeq ($(DUNE_SECONDARY),)
	cd src_ext/dune-local && ocaml bootstrap.ml
else
	cd src_ext/dune-local && ( unset OCAMLLIB ; unset CAML_LD_LIBRARY_PATH ; PATH="$(dir $(realpath $(DUNE_SECONDARY))):$$PATH" ../../$(DUNE_SECONDARY) bootstrap.ml )
endif

src_ext/dune-local.stamp:
	$(MAKE) -C src_ext dune-local.stamp

dune: $(DUNE_DEP)
	@$(DUNE) build $(DUNE_PROFILE_ARG) --root . $(DUNE_ARGS) @install

opam: $(DUNE_DEP) build-opam processed-opam.install
	@$(LN_S) -f _build/default/src/client/opamMain.exe $@$(EXE)
ifneq ($(MANIFEST_ARCH),)
	@mkdir -p Opam.Runtime.$(MANIFEST_ARCH)
	@cp -f src/manifest/Opam.Runtime.$(MANIFEST_ARCH).manifest Opam.Runtime.$(MANIFEST_ARCH)/
	@cd Opam.Runtime.$(MANIFEST_ARCH) && $(LN_S) -f ../_build/install/default/bin/Opam.Runtime.$(MANIFEST_ARCH)/libstdc++-6.dll .
	@cd Opam.Runtime.$(MANIFEST_ARCH) && $(LN_S) -f ../_build/install/default/bin/Opam.Runtime.$(MANIFEST_ARCH)/libwinpthread-1.dll .
	@cd Opam.Runtime.$(MANIFEST_ARCH) && $(LN_S) -f ../_build/install/default/bin/Opam.Runtime.$(MANIFEST_ARCH)/$(RUNTIME_GCC_S).dll .
endif

opam-installer: $(DUNE_DEP) build-opam-installer processed-opam-installer.install
	@$(LN_S) -f _build/default/src/tools/opam_installer.exe $@$(EXE)

opam-admin.top: $(DUNE_DEP)
	$(DUNE) build $(DUNE_PROFILE_ARG) --root . $(DUNE_ARGS) src/tools/opam_admin_topstart.bc
	$(LN_S) -f _build/default/src/tools/opam_admin_topstart.bc $@$(EXE)

lib-ext:
	$(MAKE) -j -C src_ext lib-ext

lib-pkg:
	$(MAKE) -j -C src_ext lib-pkg

download-ext:
	$(MAKE) -C src_ext cache-archives

download-pkg:
	$(MAKE) -C src_ext archives-pkg

clean-ext:
	$(MAKE) -C src_ext distclean

clean:
	$(MAKE) -C doc $@
	rm -f *.install *.env *.err *.info *.out opam$(EXE) opam-admin.top$(EXE) opam-installer$(EXE)
	rm -f src/client/no-git-version
	rm -rf _build Opam.Runtime.*

distclean: clean clean-ext
	rm -rf autom4te.cache bootstrap
	rm -f Makefile.config config.log config.status aclocal.m4
	rm -f src/*.META src/*/.merlin src/manifest/dune src/manifest/install.inc src/stubs/libacl/dune src/stubs/win32/dune src/stubs/win32/cc64 src/ocaml-flags-configure.sexp src/stubs/libacl/c-libraries.sexp
	rm -f src/client/linking.sexp src/stubs/c-flags.sexp src/core/developer src/core/version

OPAMINSTALLER_FLAGS = --prefix "$(call CYGPATH,$(DESTDIR)$(prefix))"
OPAMINSTALLER_FLAGS += --mandir "$(call CYGPATH,$(DESTDIR)$(mandir))"

# With ocamlfind, prefer to install to the standard directory rather
# than $(prefix) if there are no overrides
ifdef OCAMLFIND
ifndef DESTDIR
ifneq ($(OCAMLFIND),no)
    LIBINSTALL_DIR ?= $(shell PATH="$(PATH)" $(OCAMLFIND) printconf destdir)
endif
endif
endif

ifneq ($(LIBINSTALL_DIR),)
    OPAMINSTALLER_FLAGS += --libdir "$(call CYGPATH,$(LIBINSTALL_DIR))" --docdir "$(call CYGPATH,$(LIBINSTALL_DIR)/../doc)"
endif

opam-devel.install: $(DUNE_DEP)
	$(DUNE) build $(DUNE_ARGS) -p opam opam.install
	sed -e "/lib\/opam\/opam/d" -e "s/bin:/libexec:/" opam.install > $@

opam-%.install: $(DUNE_DEP)
ifeq ($(VENDORED),true)
	$(error Libraries cannot be built in vendored deps mode)
else
	$(DUNE) build $(DUNE_ARGS) -p opam-$* $@
endif

.PHONY: build-opam-installer
build-opam-installer: $(DUNE_DEP) 
	$(DUNE) build $(DUNE_PROFILE_ARG) --root . $(DUNE_ARGS) --promote-install-files -- opam-installer.install
opam-installer.install: $(DUNE_DEP)
	$(DUNE) build $(DUNE_PROFILE_ARG) --root . $(DUNE_ARGS) --promote-install-files -- opam-installer.install

.PHONY: build-opam
build-opam: $(DUNE_DEP)
	$(DUNE) build $(DUNE_PROFILE_ARG) --root . $(DUNE_ARGS) --promote-install-files -- opam-installer.install opam.install
opam.install: $(DUNE_DEP)
	$(DUNE) build $(DUNE_PROFILE_ARG) --root . $(DUNE_ARGS) --promote-install-files -- opam-installer.install opam.install

OPAMLIBS = core format solver repository state client

opam-%: $(DUNE_DEP)
	$(DUNE) build $(DUNE_PROFILE_ARG) --root . $(DUNE_ARGS) --promote-install-files -- opam-$*.install

opam-lib: $(DUNE_DEP)
	$(DUNE) build $(DUNE_PROFILE_ARG) --root . $(DUNE_ARGS) --promote-install-files -- $(patsubst %,opam-%.install,$(OPAMLIBS))

installlib-%: opam-installer opam-%.install
ifeq ($(VENDORED),true)
	$(error Installing the opam libraries is incompatible with embedding \
	        the dependencies. Run 'make clean-ext' and try again))
else
	$(OPAMINSTALLER) $(OPAMINSTALLER_FLAGS) opam-$*.install
endif

uninstalllib-%: opam-installer opam-%.install
	$(OPAMINSTALLER) -u $(OPAMINSTALLER_FLAGS) opam-$*.install

libinstall: $(DUNE_DEP) opam-admin.top $(OPAMLIBS:%=installlib-%)
	@

custom-libinstall: $(DUNE_DEP) opam-lib opam
	for p in $(OPAMLIBS); do \
	  ./opam$(EXE) custom-install --no-recompilations opam-$$p.$(version) -- \
	    $(DUNE) install --root . opam-$$p; \
	done

processed-%.install: %.install
	sed -f process.sed $^ > $@

install: processed-opam.install processed-opam-installer.install
	$(OPAMINSTALLER) $(OPAMINSTALLER_FLAGS) processed-opam.install
	$(OPAMINSTALLER) $(OPAMINSTALLER_FLAGS) processed-opam-installer.install

libuninstall: $(OPAMLIBS:%=uninstalllib-%)
	@

uninstall: opam.install
	$(OPAMINSTALLER) -u $(OPAMINSTALLER_FLAGS) $<
	$(OPAMINSTALLER) -u $(OPAMINSTALLER_FLAGS) opam-installer.install

.PHONY: test
test: tests

.PHONY: tests
tests: $(DUNE_DEP) src/client/no-git-version
	@$(DUNE) runtest $(DUNE_PROFILE_ARG) --root . $(DUNE_ARGS) src/ tests/ --no-buffer; \
	ret=$$?; \
	echo "###     TESTS RESULT SUMMARY     ###"; \
	for t in _build/default/tests/reftests/*.test; do \
	  printf "%-30s" $$(basename $$t .test); \
	  if [ ! -e $${t%.test}.out ]; \
	  then printf '\033[33m[SKIP]\033[m\n'; \
	  elif diff -q --strip-trailing-cr $$t $${t%.test}.out >/dev/null; \
	  then printf '\033[32m[ OK ]\033[m\n'; \
	  else printf '\033[31m[FAIL]\033[m\n'; \
	  fi; \
	done; \
	test $$ret -eq 0

.PHONY: crowbar
# only run the quickcheck-style tests, not very covering
crowbar: $(DUNE_DEP)
	$(DUNE) exec --root . -- src/crowbar/test.exe

.PHONY: crowbar-afl
# runs the real AFL deal, but needs to be done in a +afl switch
crowbar-afl: $(DUNE_DEP)
	$(DUNE) build --root . -- src/crowbar/test.exe
	mkdir -p /tmp/opam-crowbar-input -p /tmp/opam-crowbar-output
	echo foo > /tmp/opam-crowbar-input/foo
	afl-fuzz -i /tmp/opam-crowbar-input -o /tmp/opam-crowbar-output dune exec src/crowbar/test.exe @@

INTERMEDIATE: src/client/no-git-version
src/client/no-git-version:
	touch src/client/no-git-version

# tests-local, tests-git
tests-%: $(DUNE_DEP) src/client/no-git-version
	$(DUNE) build $(DUNE_ARGS) $(DUNE_PROFILE_ARG) --root . @reftest-legacy-$* --force

reftest-gen: src/client/no-git-version
	echo >tests/reftests/dune.inc
	$(DUNE) build $(DUNE_ARGS) $(DUNE_PROFILE_ARG) --root . @reftest-gen --auto-promote --force

reftest-runner: $(DUNE_DEP) src/client/no-git-version
	$(DUNE) build $(DUNE_ARGS) $(DUNE_PROFILE_ARG) --root . tests/reftests/run.exe

reftests: $(DUNE_DEP) src/client/no-git-version
	$(DUNE) build $(DUNE_ARGS) $(DUNE_PROFILE_ARG) --root . @reftest

reftest-%: $(DUNE_DEP) src/client/no-git-version
	$(DUNE) build $(DUNE_ARGS) $(DUNE_PROFILE_ARG) --root . @reftest-$* --force

reftests-meld:
	meld `for t in tests/reftests/*.test; do \
	  out=_build/default/$${t%.test}.out; \
	  if test -f $$out && ! diff -q $$t $$out 2> /dev/null > /dev/null; then \
	    echo --diff $$t $$out; \
	  fi; done`

.PHONY: doc
doc: all
	$(MAKE) -C doc

.PHONY: man-html
man-html: opam opam-installer
	$(MAKE) -C doc $@

configure: configure.ac m4/*.m4 shell/autogen
	shell/autogen

release-%:
	$(MAKE) -C release TAG="$*"

ifeq ($(OCAML_PORT),)
ifneq ($(COMSPEC),)
ifeq ($(shell which gcc 2>/dev/null),)
OCAML_PORT=auto
endif
endif
endif

.PHONY: compiler cold
compiler:
	env MAKE=$(MAKE) BOOTSTRAP_EXTRA_OPTS= BOOTSTRAP_TARGETS=world.opt BOOTSTRAP_ROOT=.. BOOTSTRAP_DIR=bootstrap ./shell/bootstrap-ocaml.sh $(OCAML_PORT)

src_ext/secondary/ocaml/bin/ocaml:
	env MAKE=$(MAKE) BOOTSTRAP_EXTRA_OPTS="--disable-ocamldoc --disable-debug-runtime --disable-debugger" BOOTSTRAP_TARGETS="world opt" BOOTSTRAP_ROOT=../.. BOOTSTRAP_DIR=src_ext/secondary ./shell/bootstrap-ocaml.sh $(OCAML_PORT)

cold: compiler
	env PATH="`pwd`/bootstrap/ocaml/bin:$$PATH" CAML_LD_LIBRARY_PATH= ./configure --with-vendored-deps --without-dune --enable-cold-check $(CONFIGURE_ARGS)
	env PATH="`pwd`/bootstrap/ocaml/bin:$$PATH" CAML_LD_LIBRARY_PATH= $(MAKE)

cold-%:
	env PATH="`pwd`/bootstrap/ocaml/bin:$$PATH" CAML_LD_LIBRARY_PATH= $(MAKE) $*
