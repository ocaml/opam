# Make sure to have ocp-get-server running before launchin the tests
OCPGET     = ../../ocp-get --debug
LOCALHOST ?= 127.0.0.1
ARCHIVES   = P1-1.tar.gz P2-1.tar.gz P3-1-weird.version.tar.gz P4-1.tar.gz

.PHONY: all upload

all: init upload info
	@

init: fresh
	$(OCPGET) init $(LOCALHOST)

upload: $(ARCHIVES) init
	$(OCPGET) upload P1.opam
	$(OCPGET) upload P2.opam
	$(OCPGET) upload P3.opam
	$(OCPGET) upload P4.opam

info: upload
	$(OCPGET) info

%.tar.gz: %
	tar cz $^ > $@

clean:
	rm -f $(ARCHIVES)

fresh:
	rm -rf ~/.opam ~/.opam-server