# never use /tmp
# should be absolute
TEST_DIR=/tmp
OPAM_ROOT = $(TEST_DIR)/OPAM.CLIENT
OPAM_REPO = $(TEST_DIR)/OPAM.REPO
BIN       = $(TEST_DIR)/OPAM.BIN
REPO      = test

BINARIES  = opam opam-rsync-init opam-rsync-update opam-rsync-download opam-rsync-upload
PACKAGES  = P1-1 P1-2 P2 P3 P4

# opam in the path should not be a requirement
ENV  = OCAMLRUNPARAM=b OPAMDEBUG=1 OPAM_ROOT=$(OPAM_ROOT) PATH=$(BIN):$(PATH)
OPAM = $(ENV) opam --root $(OPAM_ROOT)

BINARIES_ = $(BINARIES:%=$(BIN)/%)
BUILDS    = $(foreach bin, $(BINARIES), ../_obuild/$(bin)/$(bin).asm)
ARCHIVES  = $(PACKAGES:%=packages/%.tar.gz)

.PHONY: all upload

all: fresh init upload list install upload-new upgrade downgrade remove
	@

$(BINARIES_): $(BUILDS)
	mkdir -p $(BIN)
	for bin in $(BINARIES); do \
	  cp ../_obuild/$$bin/$$bin.asm $(BIN)/$$bin ; \
	done

init: fresh $(BIN)/opam $(SCRIPTS:%=$(BIN)/%)
	mkdir $(OPAM_REPO)
	mkdir $(OPAM_REPO)/opam
	mkdir $(OPAM_REPO)/descr
	mkdir $(OPAM_REPO)/archive
	$(OPAM) init $(REPO) $(OPAM_REPO)

upload: $(ARCHIVES) init
	cd packages && \
	$(OPAM) upload -opam P1-1.opam -descr P1-1/README -archive P1-1.tar.gz -repo $(REPO)
	cd packages && \
	$(OPAM) upload -opam P2.opam -descr P2/README -archive P2.tar.gz -repo $(REPO)
	cd packages && \
	$(OPAM) upload -opam P3.opam -descr P3/README -archive P3.tar.gz -repo $(REPO)
	cd packages && \
	$(OPAM) upload -opam P4-1.opam -descr P4/README -archive P4.tar.gz -repo $(REPO)
	$(OPAM) update # update the list of available packages with the one being upadated

list: upload
	$(OPAM) list

remove: upload
	cd packages && $(OPAM) install P1
	cd packages && $(OPAM) remove P1

install: remove upload
	$(OPAM) install P1
	$(OPAM) install P2
	$(OPAM) install P3
	$(OPAM) install P4

upload-new: install
	cd packages && \
	$(OPAM) upload -opam P1-2.opam -descr P1-2/README -archive P1-2.tar.gz -repo $(REPO)
	cd packages && \
	$(OPAM) upload -opam P4-2.opam -descr P4/README -archive P4.tar.gz -repo $(REPO)
	cd packages && \
	$(OPAM) upload -opam P4-3.opam -descr P4/README -archive P4.tar.gz -repo $(REPO)
	$(OPAM) update # update the list of available packages with the one being upadated
upgrade: upload-new
	$(OPAM) list
	$(OPAM) upgrade
	$(OPAM) list

downgrade: upgrade
	$(OPAM) install P4.2
	$(OPAM) list

packages/%.tar.gz: packages/% packages/%/*
	cd packages && tar cz $* > $*.tar.gz

clean:
	rm -f $(ARCHIVES)

fresh:
	rm -rf $(OPAM_ROOT) $(OPAM_REPO) $(BIN)
