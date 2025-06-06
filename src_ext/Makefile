ifneq ($(wildcard ../Makefile.config)$(filter-out archives cache-archives,$(MAKECMDGOALS)),)
-include ../Makefile.config
endif

ifneq ($(wildcard Makefile.config),)
include Makefile.config
endif

PATCH ?= patch

URL_ocaml = https://github.com/ocaml/ocaml/archive/refs/tags/4.14.2.tar.gz
MD5_ocaml = b28daefda803b5d5910cecf085b1451c

URL_flexdll = https://github.com/ocaml/flexdll/archive/0.43.tar.gz
MD5_flexdll = 6ce706f6c65b2c5adf5791fac678f090

ifndef FETCH
  ifneq ($(shell command -v curl 2>/dev/null),)
    FETCH = curl -LSfs -o $(2) $(1)
  else
    FETCH = wget -O $(2) $(1)
  endif
endif

SRC_EXTS = \
  cppo base64 extlib re cmdliner ocamlgraph cudf dose3 opam-file-format seq \
  stdlib-shims spdx_licenses opam-0install-cudf 0install-solver uutf \
  jsonm sha swhid_core menhir patch

ifeq ($(MCCS_ENABLED),true)
SRC_EXTS := $(SRC_EXTS) mccs
endif

include Makefile.sources

ARCHIVES = $(foreach lib,$(SRC_EXTS),$(notdir $(URL_$(lib))))
lib_of = $(foreach lib,$(SRC_EXTS),$(if $(findstring $(1),$(URL_$(lib))),$(lib),,))

ARCHIVE_FILE = $(1)$(patsubst %.tbz,.tbz,$(patsubst %.tar.gz,.tar.gz,$(URL_$(2)$(1))))

SILENT ?=

ifdef OCAML
# Portable md5check
ifeq ($(SILENT),)
MD5CHECK = $(OCAML) ../shell/md5check.ml $(1) $(2)
else
MD5CHECK = OPAM_MD5_SILENT=true $(OCAML) ../shell/md5check.ml $(1) $(2)
endif
else
MD5CHECK = { \
  sum=`md5sum $(1) 2>/dev/null | sed -e 's/^[^a-f0-9]*\([a-f0-9]*\).*/\1/'`; \
  test -n "$$sum" || sum=`md5 $(1) | sed -e 's/^.* = //'`; \
  { test "$$sum" = "$(2)" $(if $(SILENT),,&& echo '$(1) has the expected MD5.'); } || \
  { rm -f $(1); \
    echo 'MD5 for $(1) differs:'; \
    echo '  expected: $(2)'; \
    echo "    actual: $$sum"; \
    false; \
  }; \
}
endif

lib-ext:
ifeq ($(VENDORED),true)
	@echo NOTE: make lib-ext is no longer required
else
	$(error Re-run configure --with-vendored-deps)
endif

ifeq ($(DUNE),)
DUNE_DEP=dune-local/_boot/dune$(EXE)
DUNE_CLONE=dune-local.stamp
ifeq ($(shell command -v cygpath 2>/dev/null),)
DUNE:=$(DUNE_DEP)
else
DUNE:=$(shell echo "$(DUNE_DEP)" | cygpath -f - -a)
endif
else
DUNE_DEP=
DUNE_CLONE=
endif

dune-local/_boot/dune$(EXE): $(DUNE_CLONE)
	cd dune-local && ocaml boot/bootstrap.ml

.PHONY: ext-ignore
ext-ignore:
	@echo "; This file is automatically generated" > dune
	@echo "(dirs :standard \ dune-local)" >> dune
	@echo "(vendored_dirs $(SRC_EXTS))" >> dune

.PHONY: clone
clone: $(DUNE_CLONE) $(SRC_EXTS:=.stamp) | ext-ignore
	@true

.PHONY: archives
archives: $(DUNE_CLONE:.stamp=.download) $(SRC_EXTS:=.download)
	@true

cache-archives: $(SRC_EXTS:=.cache) mccs.cache dune-local.cache ocaml.cache flexdll.cache
	@

has-archives: $(addprefix archives/, $(notdir $(URL_ocaml)) $(notdir $(URL_flexdll)) $(ARCHIVES))
	@

define cache_url
https://opam.ocaml.org/cache/md5/$(shell echo $(MD5_$(2)$(1)) | cut -c -2)/$(MD5_$(2)$(1))
endef

GET_ARCHIVE=\
  { echo ' * Downloading $(1)...' && \
    { { $(call FETCH,$(URL_$(2)$(1)),$(call ARCHIVE_FILE,$(1),$(2))) && $(call MD5CHECK,$(call ARCHIVE_FILE,$(1),$(2)),$(MD5_$(2)$(1))); } \
      || { echo 'Failed to download $(URL_$(2)$(1))'; false; }; } || \
    { { $(call FETCH,$(call cache_url,$(1),$(2)),$(call ARCHIVE_FILE,$(1),$(2))) && $(call MD5CHECK,$(call ARCHIVE_FILE,$(1),$(2)),$(MD5_$(2)$(1))) && echo 'Warning: downloaded $(URL_$(2)$(1)) from opam cache'; } \
      || { echo 'Failed to download $(1) from opam cache'; false; }; }; }

%.cache:
	@mkdir -p archives
	@test -f archives/$(notdir $(URL_$*)) || \
          { $(call GET_ARCHIVE,$*) && mv $(call ARCHIVE_FILE,$*) archives/$(notdir $(URL_$*)); }

.PRECIOUS: %.download
%.download: Makefile.sources
	$(SILENT)[ -e $(call ARCHIVE_FILE,$*) ] || \
    cp archives/$(notdir $(URL_$*)) $(call ARCHIVE_FILE,$*) 2>/dev/null || $(call GET_ARCHIVE,$*)

%.stamp: %.download
	$(SILENT)mkdir -p tmp-$*
	$(SILENT)cd tmp-$* && $(if $(patsubst %.tar.gz,,$(URL_$*)),bunzip2,gunzip) -c ../$(call ARCHIVE_FILE,$*) | tar xf -
	$(SILENT)rm -rf $*
	@for ii in tmp-$*/*; do if [ -d $${ii} ]; then mv $${ii} $*; fi; done; \
   rm -rf tmp-$*
	@if [ -d patches/$* ]; then \
          cd $* && \
     for p in ../patches/$*/*.patch; do \
       $(PATCH) -p1 < $$p $(if $(SILENT),> /dev/null); \
     done; \
   fi
	@if [ "$*" != "dune-local" ] ; then \
     for j in $(wildcard dune-$* dune-$*-*); do \
       cp $$j $*$$(echo "$$j" | sed -e "s/dune-$*//" -e "s|-|/|g")/dune; \
       echo "(lang dune 1.2)" > $*/dune-project; \
       echo "(name $*)" >> $*/dune-project; \
       touch $*/$*.opam; \
     done; \
   fi
	@touch $@

clean:
	@

distclean: clean
	rm -rf dune-local secondary $(SRC_EXTS)
	rm -f *.tar.gz *.tbz *.*stamp *.*download dune \
        Makefile.config
	[ -d archives ] && ([ "$$(find archives -maxdepth 0 -type d -empty)" != "" ] && rmdir archives || echo "WARNING! $$(pwd)/archives/ not empty so left") || true
