-include Makefile.config

all: opam-lib opam opam-admin opam-installer

#backwards-compat
compile with-ocamlbuild: all
install-with-ocamlbuild: install
libinstall-with-ocamlbuild: libinstall

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

libinstall: opam-installer opam-lib
	$(if $(wildcard src_ext/lib/*),$(error Installing the opam libraries is incompatible with embedding the dependencies. Run 'make clean-ext' and try again))
	src/opam-installer --prefix $(prefix) opam-lib.install

install: opam-installer opam
	src/opam-installer --prefix $(prefix) opam.install

libuninstall: opam-installer
	src/opam-installer -u --prefix $(prefix) opam-lib.install

uninstall: opam-installer
	src/opam-installer -u --prefix $(prefix) opam.install

tests:
	$(MAKE) -C tests all

tests-local:
	$(MAKE) -C tests local

tests-git:
	$(MAKE) -C tests git

doc: opam-lib
	$(MAKE) -C doc

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

fast: src/core/opamGitVersion.ml src/core/opamScript.ml
	@if [ -n "$(wildcard src/*/*.cmi)" ]; then $(MAKE) clean; fi
	@ocp-build -init -scan
	@ln -sf _obuild/opam/opam.asm src/opam
	@ln -sf _obuild/opam-admin/opam-admin.asm src/opam-admin
	@ln -sf _obuild/opam-installer/opam-installer.asm src/opam-installer
	@ln -sf _obuild/opam-check/opam-check.asm src/opam-check

fastclean:
	@ocp-build -clean
	@rm -f $(addprefix src/, opam opam-admin opam-installer opam-check)
