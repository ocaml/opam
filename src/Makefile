ifeq ($(findstring clean,$(MAKECMDGOALS)),)
-include ../Makefile.config

ifndef version
  $(error Please run ./configure)
endif
endif

ifeq ($(JBUILDER),)
JBUILDER_LOCAL=../src_ext/jbuilder/_build/install/default/bin/jbuilder$(EXE)
ifeq ($(shell command -v cygpath 2>/dev/null),)
JBUILDER:=$(JBUILDER_LOCAL)
else
JBUILDER:=$(shell echo "$(JBUILDER_LOCAL)" | cygpath -f - -a)
endif
endif

all:
	$(JBUILDER) build @install
	$(JBUILDER) build tools/opam_check.exe

# --

SUBS = core format solver repository state client
OPAMLIB = $(foreach lib,$(SUBS),$(lib)/opam_$(lib))

opam-lib.byte:
	$(JBUILDER) build $(OPAMLIB:=.cma)

opam-lib.native:
	$(JBUILDER) build $(OPAMLIB:=.cmxa)

opam-lib: opam-lib.byte $(if $(USE_BYTE),,opam-lib.native) opam-admin.top
	@

opam:
	$(JBUILDER) build ../opam.install

opam-installer:
	$(JBUILDER) build tools/opam_installer.exe

opam-admin.top:
	$(JBUILDER) build tools/opam_admin_top.bc

clean:
