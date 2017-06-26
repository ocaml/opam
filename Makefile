ifeq ($(findstring clean,$(MAKECMDGOALS)),)
-include Makefile.config
endif

all: opam-lib opam opam-installer
	@

ALWAYS:
	@

opam-lib opam opam-installer all: ALWAYS

#backwards-compat
compile with-ocamlbuild: all
	@
install-with-ocamlbuild: install
	@
libinstall-with-ocamlbuild: libinstall
	@

byte:
	$(MAKE) all USE_BYTE=true

src/%:
	$(MAKE) -C src $*

# Disable this rule if the only build targets are cold, download-ext or configure
# to suppress error messages trying to build Makefile.config
ifneq ($(or $(filter-out cold download-ext configure,$(MAKECMDGOALS)),$(filter own-goal,own-$(MAKECMDGOALS)goal)),)
%:
	$(MAKE) -C src $@
endif

lib-ext:
	$(MAKE) -j -C src_ext lib-ext

download-ext:
	$(MAKE) -C src_ext archives

clean-ext:
	$(MAKE) -C src_ext distclean

clean:
	$(MAKE) -C src $@
	$(MAKE) -C doc $@
	rm -f *.install *.env *.err *.info *.out
	rm -rf _obuild

distclean: clean clean-ext
	rm -rf autom4te.cache bootstrap
	rm -f .merlin Makefile.config config.log config.status src/core/opamVersion.ml src/core/opamCoreConfig.ml aclocal.m4
	rm -f src/*.META

OPAMINSTALLER_FLAGS = --prefix "$(DESTDIR)$(prefix)"
OPAMINSTALLER_FLAGS += --mandir "$(DESTDIR)$(mandir)"

# With ocamlfind, prefer to install to the standard directory rather
# than $(prefix) if there are no overrides
ifdef OCAMLFIND
ifndef DESTDIR
ifneq ($(OCAMLFIND),no)
    LIBINSTALL_DIR ?= $(shell $(OCAMLFIND) printconf destdir)
endif
endif
endif

ifneq ($(LIBINSTALL_DIR),)
    OPAMINSTALLER_FLAGS += --libdir "$(LIBINSTALL_DIR)"
endif

opam-%.install: ALWAYS
	$(MAKE) -C src ../opam-$*.install

opam.install:
	@echo 'bin: [' >$@
	@echo '  "src/opam$(EXE)"' >>$@
	@echo '  "src/opam-installer$(EXE)"' >>$@
	@echo ']' >>$@
	@echo 'man: [' >>$@
	@$(patsubst %,echo '  "'%'"' >>$@;,$(wildcard doc/man/*.1))
	@echo ']' >>$@
	@echo 'doc: [' >>$@
	@$(foreach x,$(wildcard doc/man-html/*.html),\
	  echo '  "$x" {"man/$(notdir $x)"}' >>$@;)
	@$(foreach x,$(wildcard doc/pages/*.html),\
	  echo '  "$x" {"$(notdir $x)"}' >>$@;)
	@echo ']' >>$@

OPAMLIBS = core format solver repository state client

installlib-%: opam-installer opam-%.install src/opam-%$(LIBEXT)
	$(if $(wildcard src_ext/lib/*),\
	  $(error Installing the opam libraries is incompatible with embedding \
	          the dependencies. Run 'make clean-ext' and try again))
	src/opam-installer $(OPAMINSTALLER_FLAGS) opam-$*.install

uninstalllib-%: opam-installer opam-%.install src/opam-%$(LIBEXT)
	src/opam-installer -u $(OPAMINSTALLER_FLAGS) opam-$*.install

libinstall: opam-admin.top $(OPAMLIBS:%=installlib-%)
	@

install: opam.install
	src/opam-installer $(OPAMINSTALLER_FLAGS) $<

libuninstall: $(OPAMLIBS:%=uninstalllib-%)
	@

uninstall: opam.install
	src/opam-installer -u $(OPAMINSTALLER_FLAGS) opam.install

.PHONY: tests tests-local tests-git
tests:
	$(MAKE) -C tests all

# tests-local, tests-git
tests-%:
	$(MAKE) -C tests $*

.PHONY: doc
doc: all
	$(MAKE) -C doc

.PHONY: man man-html
man man-html: opam opam-installer
	$(MAKE) -C doc $@

configure: configure.ac m4/*.m4
	aclocal -I m4
	autoconf

release-tag:
	git tag -d latest || true
	git tag -a latest -m "Latest release"
	git tag -a $(version) -m "Release $(version)"

fastclean:: rmartefacts

include Makefile.ocp-build

rmartefacts: ALWAYS
	@rm -f $(addprefix src/, opam opam-installer opam-check)
	@$(foreach l,core format solver repository state client tools,\
	   $(foreach e,a cma cmxa,rm -f src/opam-$l.$e;)\
	   $(foreach e,o cmo cmx cmxs cmi cmt cmti,rm -f $(wildcard src/$l/*.$e);))

cold:
	./shell/bootstrap-ocaml.sh
	env PATH="`pwd`/bootstrap/ocaml/bin:$$PATH" ./configure $(CONFIGURE_ARGS)
	env PATH="`pwd`/bootstrap/ocaml/bin:$$PATH" $(MAKE) lib-ext
	env PATH="`pwd`/bootstrap/ocaml/bin:$$PATH" $(MAKE)
