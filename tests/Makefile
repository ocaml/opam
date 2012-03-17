# Make sure to have ocp-get-server running before launchin the tests
LOCALHOST       ?= 127.0.0.1
OPAM_ROOT        = /tmp/OPAM.TEST
OPAM_SERVER_ROOT = /tmp/OPAM.SERVER.TEST
BIN              = /tmp

ENV           = export OCAMLRUNPARAM=b PATH=$(BIN):$(PATH) &&
OCPGET        = $(ENV) ocp-get --debug --root $(OPAM_ROOT)
OCPGET_SERVER = $(ENV) ocp-get-server --debug --root $(OPAM_SERVER_ROOT)

PACKAGES   = P1-1 P2-1 P2-1 P3-1-weird.version P4-1

ARCHIVES = $(PACKAGES:%=packages/%.tar.gz)

.PHONY: all upload

all: init upload info install
	@

install-binaries:
	cp ../ocp-get ../ocp-get-server $(BIN)

runserver: install-binaries
	$(OCPGET_SERVER)

init: install-binaries fresh
	$(OCPGET) init $(LOCALHOST)

upload: $(ARCHIVES) init
	cd packages && $(OCPGET) upload P1
	cd packages && $(OCPGET) upload P2
	cd packages && $(OCPGET) upload P3
	cd packages && $(OCPGET) upload P4

info: upload
	$(OCPGET) info

install: upload
	$(OCPGET) install P1
	$(OCPGET) install P2
	$(OCPGET) install P3
	$(OCPGET) install P4

upload-new: install
	cd packages && \
	  cp P1.opam P1.opam.1 && cp P1.opam.2 P1.opam && \
	  $(OCPGET) upload P1 && \
	  cp P1.opam.1 P1.opam

updgrade: upload-new
	$(OCPGET) upgrade

packages/%.tar.gz: packages/% packages/%/*
	cd packages && tar cz $* > $*.tar.gz

clean:
	rm -f $(ARCHIVES)

fresh:
	rm -rf $(OPAM_ROOT) $(OPAM_SERVER_ROOT)