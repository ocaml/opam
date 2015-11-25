-include Makefile.config

all: opam-lib opam opam-admin opam-installer
	@

ALWAYS:
	@

opam-lib opam opam-admin opam-installer all: ALWAYS

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

%:
	$(MAKE) -C src $@

lib-ext:
	$(MAKE) -C src_ext lib-ext

download-ext:
	$(MAKE) -C src_ext archives

clean-ext:
	$(MAKE) -C src_ext distclean

clean:
	$(MAKE) -C src $@
	$(MAKE) -C doc $@

OPAMINSTALLER_FLAGS = --prefix $(DESTDIR)$(prefix)
OPAMINSTALLER_FLAGS += --mandir $(DESTDIR)$(mandir)

# With ocamlfind, prefer to install to the standard directory rather
# than $(prefix) if there are no overrides
ifndef DESTDIR
ifneq ($(OCAMLFIND),no)
    LIBINSTALL_DIR ?= $(shell $(OCAMLFIND) printconf destdir)
endif
endif

ifneq ($(LIBINSTALL_DIR),)
    OPAMINSTALLER_FLAGS += --libdir $(LIBINSTALL_DIR)
endif

opam-lib.install:
	$(MAKE) -C src ../opam-lib.install

libinstall: opam-lib.install opam-admin.top
	$(if $(wildcard src_ext/lib/*),$(error Installing the opam libraries is incompatible with embedding the dependencies. Run 'make clean-ext' and try again))
	src/opam-installer $(OPAMINSTALLER_FLAGS) opam-lib.install

install:
	src/opam-installer $(OPAMINSTALLER_FLAGS) opam.install

libuninstall:
	src/opam-installer -u $(OPAMINSTALLER_FLAGS) opam-lib.install

uninstall:
	src/opam-installer -u $(OPAMINSTALLER_FLAGS) opam.install

.PHONY: tests tests-local tests-git
tests: opam opam-admin opam-check
	$(MAKE) -C tests all

# tests-local, tests-git
tests-%: opam opam-admin opam-check
	$(MAKE) -C tests $*

.PHONY: doc
doc: all
	$(MAKE) -C doc

.PHONY: man man-html
man man-html: opam opam-admin opam-installer
	$(MAKE) -C doc $@

configure: configure.ac m4/*.m4
	aclocal -I m4
	autoconf

release-tag:
	git tag -d latest || true
	git tag -a latest -m "Latest release"
	git tag -a $(version) -m "Release $(version)"


$(OPAM_FULL).tar.gz:
	$(MAKE) -C src_ext distclean
	$(MAKE) -C src_ext downloads
	rm -f $(OPAM_FULL) $(OPAM_FULL).tar.gz
	ln -s .

fastlink:
	@$(foreach b,opam opam-admin opam-installer opam-check,\
	   ln -sf ../_obuild/$b/$b.asm src/$b;)
	@$(foreach l,core format solver repository state client,\
	   $(foreach e,a cma cmxa,ln -sf ../_obuild/opam-$l/opam-$l.$e src/opam-$l.$e;)\
	   ln -sf $(addprefix ../../,\
	        $(foreach e,o cmo cmx cmxs cmi cmt cmti,$(wildcard _obuild/opam-$l/*.$e)))\
	      src/$l/;)
	@ln -sf ../_obuild/opam-admin.top/opam-admin.top.byte src/opam-admin.top
	@ln -sf  $(addprefix ../../,\
	      $(foreach e,o cmo cmx cmxs cmi cmt cmti,$(wildcard _obuild/opam-admin.top/*.$e)))\
	      src/tools/;

rmartefacts: ALWAYS
	@rm -f $(addprefix src/, opam opam-admin opam-installer opam-check)
	@$(foreach l,core format solver repository state client,\
	   $(foreach e,a cma cmxa,rm -f src/opam-$l.$e;)\
	   $(foreach e,o cmo cmx cmxs cmi cmt cmti,rm -f $(wildcard src/$l/*.$e);))

prefast: rmartefacts src/client/opamGitVersion.ml src/state/opamScript.ml src/core/opamCompat.ml src/core/opamCompat.mli
	@ocp-build -init

fast: prefast
	@ocp-build
	@$(MAKE) fastlink

fastclean: rmartefacts
	@ocp-build -clean 2>/dev/null || ocp-build clean 2>/dev/null

cold:
	./shell/bootstrap-ocaml.sh
	env PATH="$$PATH:`pwd`/bootstrap/ocaml/bin" ./configure $(CONFIGURE_ARGS)
	env PATH="$$PATH:`pwd`/bootstrap/ocaml/bin" $(MAKE) lib-ext
	env PATH="$$PATH:`pwd`/bootstrap/ocaml/bin" $(MAKE)
