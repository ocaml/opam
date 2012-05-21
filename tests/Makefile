# never use /tmp
# should be absolute
TEST_DIR=/tmp
OPAM_ROOT = $(TEST_DIR)/OPAM.ROOT
OPAM_REPO = $(TEST_DIR)/OPAM.REPO
BIN       = $(TEST_DIR)/OPAM.BIN
REPO      = test
REPOKIND ?= rsync

# To test OPAM server repositories
OPAM_SERVER_ROOT = $(TEST_DIR)/OPAM.SERVER
OPAM_SERVER      = 127.0.0.1

# To test GIT repo
OPAM_GIT = $(TEST_DIR)/OPAM.GIT

BINARIES  = opam opam-server \
	    opam-rsync-init opam-rsync-update opam-rsync-download opam-rsync-upload \
	    opam-server-init opam-server-update opam-server-download opam-server-upload \
	    opam-git-init opam-git-update opam-git-download opam-git-upload
PACKAGES  = P1-1 P1-2 P2 P3 P4

# opam in the path should not be a requirement
ENV  = OCAMLRUNPARAM=b OPAMDEBUG=1 OPAM_ROOT=$(OPAM_ROOT) PATH=$(BIN):$(PATH)
OPAM = $(ENV) opam --root $(OPAM_ROOT)

BINARIES_ = $(BINARIES:%=$(BIN)/%)
BUILDS    = $(foreach bin, $(BINARIES), ../_obuild/$(bin)/$(bin).asm)
ARCHIVES  = $(PACKAGES:%=packages/%.tar.gz)

.PHONY: fresh init upload list install upload-new upgrade downgrade remove switch

all:
	$(MAKE) fresh && \
	$(MAKE) $(BINARIES_) && \
	$(MAKE) init && \
	$(MAKE) list && \
	$(MAKE) upload && \
	$(MAKE) install-remove && \
	$(MAKE) list && \
	$(MAKE) install && \
	$(MAKE) list && \
	$(MAKE) upload-new && \
	$(MAKE) list && \
	$(MAKE) upgrade && \
	$(MAKE) list && \
	$(MAKE) downgrade && \
	$(MAKE) list 

server:
	$(MAKE) fresh
	$(MAKE) binaries
	rm -rf $(OPAM_SERVER_ROOT)
	($(ENV) opam-server --root $(OPAM_SERVER_ROOT) & \
	(sleep 1 && $(MAKE) REPOKIND=server OPAM_REPO=$(OPAM_SERVER) all \
	  && killall opam-server || killall opam-server))

git:
	$(MAKE) REPOKIND=git all

$(BINARIES_): $(BUILDS)
	mkdir -p $(BIN)
	for bin in $(BINARIES); do \
	  cp ../_obuild/$$bin/$$bin.asm $(BIN)/$$bin ; \
	done

init:
ifeq ($(REPOKIND),rsync)
	mkdir -p $(OPAM_REPO)
	mkdir $(OPAM_REPO)/opam
	mkdir $(OPAM_REPO)/descr
	mkdir $(OPAM_REPO)/archive
endif
ifeq ($(REPOKIND), git)
	rm -rf $(OPAM_REPO)
	mkdir -p $(OPAM_REPO)
	mkdir $(OPAM_REPO)/opam
	mkdir $(OPAM_REPO)/descr
	mkdir $(OPAM_REPO)/url
	cd $(OPAM_REPO) && git init && \
	  touch README && git add README && \
	  git commit -a -m "Initial commit"
endif
	$(OPAM) init $(REPO) $(OPAM_REPO) --kind $(REPOKIND)

upload: $(ARCHIVES)
ifeq ($(REPOKIND), git)
	cp packages/P1-1.opam     $(OPAM_REPO)/opam/P1.1.opam
	cp packages/P1-1/README   $(OPAM_REPO)/descr/P1.1
	echo '$(OPAM_GIT)/P1-1' > $(OPAM_REPO)/url/P1.1
	cd $(OPAM_REPO) && git add * && git commit -a -m "Adding P1"
	cp packages/P2/README     $(OPAM_REPO)/descr/P2.1
	cp packages/P2.opam       $(OPAM_REPO)/opam/P2.1.opam
	echo '$(OPAM_GIT)/P2'   > $(OPAM_REPO)/url/P2.1
	cd $(OPAM_REPO) && git add * && git commit -a -m "Adding P2"
	cp packages/P3.opam       $(OPAM_REPO)/opam/P3.1~weird-version.test.opam
	cp packages/P3/README     $(OPAM_REPO)/descr/P3.1~weird-version.test
	echo '$(OPAM_GIT)/P3'   > $(OPAM_REPO)/url/P3.1~weird-version.test
	cd $(OPAM_REPO) && git add * && git commit -a -m "Adding P3"
	cp packages/P4-1.opam     $(OPAM_REPO)/opam/P4.1.opam
	cp packages/P4/README     $(OPAM_REPO)/descr/P4.1
	echo '$(OPAM_GIT)/P4'   > $(OPAM_REPO)/url/P4.1
	cd $(OPAM_REPO) && git add * && git commit -a -m "Adding P4"
	rm -rf $(OPAM_GIT) && mkdir -p $(OPAM_GIT)
	mkdir $(OPAM_GIT)/P1-1 && cp packages/P1-1/* $(OPAM_GIT)/P1-1/
	mkdir $(OPAM_GIT)/P2   && cp packages/P2/*   $(OPAM_GIT)/P2/
	mkdir $(OPAM_GIT)/P3   && cp packages/P3/*   $(OPAM_GIT)/P3/
	mkdir $(OPAM_GIT)/P4   && cp packages/P4/*   $(OPAM_GIT)/P4/
	cd $(OPAM_GIT)/P1-1 && git init && git add * && git commit -a -m "initial commit"
	cd $(OPAM_GIT)/P2   && git init && git add * && git commit -a -m "initial commit"
	cd $(OPAM_GIT)/P3   && git init && git add * && git commit -a -m "initial commit"
	cd $(OPAM_GIT)/P4   && git init && git add * && git commit -a -m "initial commit"
else
	cd packages && \
	$(OPAM) upload -opam P1-1.opam -descr P1-1/README -archive P1-1.tar.gz -repo $(REPO)
	cd packages && \
	$(OPAM) upload -opam P2.opam -descr P2/README -archive P2.tar.gz -repo $(REPO)
	cd packages && \
	$(OPAM) upload -opam P3.opam -descr P3/README -archive P3.tar.gz -repo $(REPO)
	cd packages && \
	$(OPAM) upload -opam P4-1.opam -descr P4/README -archive P4.tar.gz -repo $(REPO)
endif
	$(OPAM) update # update the list of available packages with the one being updated

list:
	$(OPAM) list

install-remove:
	cd packages && $(OPAM) install P1
	cd packages && $(OPAM) remove P1

install:
	$(OPAM) install P1 && \
	$(OPAM) install P2 && \
	$(OPAM) install P3 && \
	$(OPAM) install P4

upload-new:
ifeq ($(REPOKIND), git)
	echo "(* new line *)" >> $(OPAM_GIT)/P1-1/p1.ml
	cd $(OPAM_GIT)/P1-1 && git commit -a -m "a small change"
	cp packages/P4-2.opam   $(OPAM_REPO)/opam/P4.2.opam
	cp packages/P4/README   $(OPAM_REPO)/descr/P4.2
	echo '$(OPAM_GIT)/P4' > $(OPAM_REPO)/url/P4.2
	cp packages/P4-3.opam   $(OPAM_REPO)/opam/P4.3.opam
	cp packages/P4/README   $(OPAM_REPO)/descr/P4.3
	echo '$(OPAM_GIT)/P4' > $(OPAM_REPO)/url/P4.3
	cd $(OPAM_REPO) && git add * && git commit -a -m "Adding P4.2 and P4.3"
else
	cd packages && \
	$(OPAM) upload -opam P1-2.opam -descr P1-2/README -archive P1-2.tar.gz -repo $(REPO)
	cd packages && \
	$(OPAM) upload -opam P4-2.opam -descr P4/README -archive P4.tar.gz -repo $(REPO)
	cd packages && \
	$(OPAM) upload -opam P4-3.opam -descr P4/README -archive P4.tar.gz -repo $(REPO)
endif
	$(OPAM) update # update the list of available packages with the one being updated

upgrade:
	$(OPAM) upgrade

downgrade:
	$(OPAM) install P4.2

switch:
	mkdir -p /tmp/opam-archives && wget -P /tmp/opam-archives -qN http://caml.inria.fr/pub/distrib/ocaml-3.11/ocaml-3.11.2.tar.gz
	mkdir -p $(OPAM_ROOT)/compilers
	cp packages/3.11.2.comp $(OPAM_ROOT)/compilers
	$(OPAM) switch 3.11.2

packages/%.tar.gz: packages/% packages/%/*
	cd packages && tar cz $* > $*.tar.gz

clean:
	rm -f $(ARCHIVES)

fresh:
	rm -rf $(OPAM_ROOT) $(OPAM_REPO) $(BIN)
