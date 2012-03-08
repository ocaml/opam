# Make sure to have ocp-get-server running before launchin the tests
LOCALHOST       ?= 127.0.0.1
OPAM_ROOT        = /tmp/OPAM.TEST
OPAM_SERVER_ROOT = /tmp/OPAM.SERVER.TEST

OCPGET        = ../ocp-get --debug --root $(OPAM_ROOT)
OCPGET_SERVER = ../ocp-get-server --debug --root $(OPAM_SERVER_ROOT)

BACKTRACES = export OCAMLRUNPARAM=b &&

PACKAGES   = P1-1 P2-1 P3-1-weird.version P4-1

ARCHIVES = $(PACKAGES:%=packages/%.tar.gz)
.PHONY: all upload

all: init upload info
	@

runserver:
	$(OCPGET_SERVER)

init: fresh
	$(OCPGET) init $(LOCALHOST)

upload: $(ARCHIVES) init
	cd packages && $(BACKTRACES) ../$(OCPGET) upload P1
	cd packages && $(BACKTRACES) ../$(OCPGET) upload P2
	cd packages && $(BACKTRACES) ../$(OCPGET) upload P3
	cd packages && $(BACKTRACES) ../$(OCPGET) upload P4

info: upload
	$(OCPGET) info

packages/%.tar.gz: packages/%
	cd packages && tar cz $* > $*.tar.gz

clean:
	rm -f $(ARCHIVES)

fresh:
	rm -rf $(OPAM_ROOT) $(OPAM_SERVER_ROOT)