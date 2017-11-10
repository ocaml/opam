ifneq ($(filter-out archives cache-archives,$(MAKECMDGOALS)),)
-include ../Makefile.config
endif

SRC_EXTS = cppo extlib re cmdliner ocamlgraph cudf dose3 opam-file-format result
ifneq ($(MCCS_DISABLED),true)
SRC_EXTS := $(SRC_EXTS) mccs
endif

URL_cppo = https://github.com/mjambon/cppo/archive/v1.5.0.tar.gz
MD5_cppo = bdc99442945f6bc26e7a8096d0975239

URL_extlib = https://github.com/ygrek/ocaml-extlib/releases/download/1.7.2/extlib-1.7.2.tar.gz
MD5_extlib = 0f550dd06242828399a73387c49e0eed

URL_re = https://github.com/ocaml/ocaml-re/archive/1.7.1.tar.gz
MD5_re = 0e45743512b7ab5e6b175f955dc72002

URL_cmdliner = http://erratique.ch/software/cmdliner/releases/cmdliner-1.0.0.tbz
MD5_cmdliner = 6baa375f393fbe9ca1fcb0a7a875e001

URL_ocamlgraph = http://ocamlgraph.lri.fr/download/ocamlgraph-1.8.7.tar.gz
MD5_ocamlgraph = e733b8309b9374e89d96e907ecaf4f76

URL_cudf = https://gforge.inria.fr/frs/download.php/file/36602/cudf-0.9.tar.gz
MD5_cudf = a4c0e652e56e74c7b388a43f9258d119

URL_dose3 = https://gforge.inria.fr/frs/download.php/file/36063/dose3-5.0.1.tar.gz
MD5_dose3 = e7d4b1840383c6732f29a47c08ba5650

URL_mccs = https://github.com/AltGr/ocaml-mccs/archive/1.1+4.tar.gz
MD5_mccs = 1ff2d73ab5c69e1fb58160213fc2d4c9

URL_opam-file-format = https://github.com/ocaml/opam-file-format/archive/2.0.0-beta3.tar.gz
MD5_opam-file-format = fb461d14a44aac3a43751aa936e79143

URL_result = https://github.com/janestreet/result/archive/1.2.tar.gz
MD5_result = 3d5b66c5526918f0f2ca9d6811ef09c8

URL_jbuilder = https://github.com/janestreet/jbuilder/archive/08050696fb701dafcd1372aadfaa800a50bc01ca.tar.gz
MD5_jbuilder = 53b65232ebb5fd319acf90922342615d

ifndef FETCH
  ifneq ($(shell command -v curl 2>/dev/null),)
    FETCH = curl -OLSs
  else
    FETCH = wget
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

lib-ext: clone
	@

ifeq ($(JBUILDER),)
JBUILDER_DEP=jbuilder/_build/install/default/bin/jbuilder$(EXE)
JBUILDER_CLONE=jbuilder.stamp
ifeq ($(shell command -v cygpath 2>/dev/null),)
JBUILDER:=$(JBUILDER_DEP)
else
JBUILDER:=$(shell echo "$(JBUILDER_DEP)" | cygpath -f - -a)
endif
else
JBUILDER_DEP=
JBUILDER_CLONE=
endif

jbuilder/_build/install/default/bin/jbuilder$(EXE): $(JBUILDER_CLONE)
	cd jbuilder && ocaml bootstrap.ml && ./boot.exe

clone: $(JBUILDER_CLONE) $(SRC_EXTS:=.stamp)
	@

.PHONY: archives
archives: $(SRC_EXTS:=.download)
	@

cache-archives: $(SRC_EXTS:=.cache) jbuilder.cache
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
	@for j in $(wildcard jbuild-$* jbuild-$*-*); do \
     cp $$j $*$$(echo "$$j" | sed -e "s/jbuild-$*//" -e "s|-|/|g")/jbuild; \
   done
	@touch $*/$*.opam
	@touch $@

clean:
	@

distclean: clean
	rm -rf jbuilder $(SRC_EXTS) mccs
	rm -f *.tar.gz *.tbz *.stamp *.download
	[ -d archives ] && ([ "$$(find archives -maxdepth 0 -type d -empty)" != "" ] && rmdir archives || echo "WARNING! $$(pwd)/archives/ not empty so left") || true
