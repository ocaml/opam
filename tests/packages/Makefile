# Make sure to have ocp-get-server running before launchin the tests
OCPGET   = ../../ocp-get --debug

ARCHIVES = P1-1.tar.gz P2-1.tar.gz P3-1-weird.version.tar.gz P4-1.tar.gz

.PHONY: all upload

all: upload
	@

upload: $(ARCHIVES)
	$(OCPGET) upload P1
	$(OCPGET) upload P2
	$(OCPGET) upload P3
	$(OCPGET) upload P4

%.tar.gz: %
	tar cz $^ > $@

clean:
	rm -f $(ARCHIVES)