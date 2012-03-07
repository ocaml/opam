# Make sure to have ocp-get-server running before launchin the tests
OCPGET     = ../ocp-get --debug
LOCALHOST ?= 127.0.0.1
PACKAGES   = P1-1 P2-1 P3-1-weird.version P4-1

ARCHIVES = $(PACKAGES:%=packages/%.tar.gz)
.PHONY: all upload

all: init upload info
	@

init: fresh
	$(OCPGET) init $(LOCALHOST)

upload: $(ARCHIVES) init
	cd packages && ../$(OCPGET) upload P1.opam
	cd packages && ../$(OCPGET) upload P2.opam
	cd packages && ../$(OCPGET) upload P3.opam
	cd packages && ../$(OCPGET) upload P4.opam

info: upload
	$(OCPGET) info

packages/%.tar.gz: packages/%
	cd packages && tar cz $* > $*.tar.gz

clean:
	rm -f $(ARCHIVES)

fresh:
	rm -rf ~/.opam ~/.opam-server