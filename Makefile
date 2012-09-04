BIN = /usr/local/bin
LOCAL_OCPBUILD=./ocp-build/ocp-build
OCPBUILD ?= $(LOCAL_OCPBUILD)
OCAMLC=ocamlc
SRC_EXT=src_ext
TARGETS = opam opam-mk-repo opam-repo-convert-0.3

.PHONY: all

all: $(LOCAL_OCPBUILD) META
	$(MAKE) clone
	$(MAKE) compile

scan: $(LOCAL_OCPBUILD)
	$(OCPBUILD) -scan
sanitize: $(LOCAL_OCPBUILD)
	$(OCPBUILD) -sanitize
byte: $(LOCAL_OCPBUILD)
	$(OCPBUILD) -byte
opt: $(LOCAL_OCPBUILD)
	$(OCPBUILD) -asm

$(LOCAL_OCPBUILD): ocp-build/ocp-build.boot ocp-build/win32_c.c
	$(MAKE) -C ocp-build

compile: $(LOCAL_OCPBUILD)
	$(OCPBUILD) -init -scan -sanitize $(TARGET)

clone:
	$(MAKE) -C $(SRC_EXT)

clean:
	rm -rf _obuild
	rm -rf src/*.annot bat/*.annot
	rm -f opam
	rm -f ocp-build.*
	$(MAKE) -C $(SRC_EXT) clean
	$(MAKE) -C ocp-build clean

distclean: clean
	rm -f *.tar.gz *.tar.bz2
	rm -rf _obuild _build
	$(MAKE) -C $(SRC_EXT) distclean

.PHONY: tests

tests:
	$(MAKE) -C tests all

tests-rsync:
	$(MAKE) -C tests rsync

tests-git:
	$(MAKE) -C tests git

%-install:
	cp _obuild/$*/$*.asm $(BIN)/$*

PRODUCT_VERSION=$(shell grep "let version" src/globals.ml | cut -f 2 -d \")
META: META.in
	sed 's/@VERSION@/$(PRODUCT_VERSION)/g' < $< > $@

.PHONY: install
install:
	$(MAKE) $(TARGETS:%=%-install)

doc: compile
	mkdir -p doc/html/
	ocamldoc \
	  -I _obuild/opam-lib -I _obuild/cudf -I _obuild/dose \
	  -I _obuild/bat -I _obuild/unix -I _obuild/extlib \
	  -I _obuild/arg -I _obuild/graph \
	  src/*.mli -html -d doc/html/

trailing:
	find src -name "*.ml*" -exec \
	  sed -i xxx -e :a -e "/^\n*$$/{$$d;N;ba" -e '}' {} \;
	find src -name "*xxx" -exec rm {} \;
