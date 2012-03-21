# Make sure to have ocp-get-server running before launchin the tests
LOCALHOST       ?= 127.0.0.1
OPAM_ROOT        = /tmp/OPAM.TEST
OPAM_SERVER_ROOT = /tmp/OPAM.SERVER.TEST
BIN              = /tmp

ENV           = export OCAMLRUNPARAM=b OPAM_ROOT=$(OPAM_ROOT) PATH=$(BIN):$(PATH) &&
OCPGET        = $(ENV) ocp-get --debug --root $(OPAM_ROOT)
OCPGET_SERVER = $(ENV) ocp-get-server --debug --root $(OPAM_SERVER_ROOT)

PACKAGES   = P1-1 P1-2 P2-1 P2-1 P3-1-weird.version P4-1

ARCHIVES = $(PACKAGES:%=packages/%.tar.gz)

.PHONY: all upload

all: fresh init upload info install upload-new upgrade
	@

$(BIN)/ocp-get: ../ocp-get
	cp ../ocp-get $(BIN)/ocp-get

$(BIN)/ocp-get-server: ../ocp-get-server
	cp ../ocp-get-server $(BIN)/ocp-get-server

runserver: fresh $(BIN)/ocp-get-server
	$(OCPGET_SERVER)

init: fresh $(BIN)/ocp-get
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
	  cp P1.spec P1.spec.1 && cp P1.spec.2 P1.spec && \
	  $(OCPGET) upload P1 && \
	  cp P1.spec.1 P1.spec

upgrade: upload-new
	$(OCPGET) info
	$(OCPGET) upgrade
	$(OCPGET) info

packages/%.tar.gz: packages/% packages/%/*
	cd packages && tar cz $* > $*.tar.gz

clean:
	rm -f $(ARCHIVES)

fresh:
	rm -rf $(OPAM_ROOT) $(OPAM_SERVER_ROOT)