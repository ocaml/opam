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

BINARIES  = opam opam-server opam-check \
	    opam-rsync-init opam-rsync-update opam-rsync-download opam-rsync-upload \
	    opam-server-init opam-server-update opam-server-download opam-server-upload \
	    opam-git-init opam-git-update opam-git-download opam-git-upload
PACKAGES  = P1-1 P1-2 P2 P3 P4 P5

OCAMLVERSION = $(shell ocamlc -version)

# opam in the path should not be a requirement
ENV  = OCAMLRUNPARAM=b OPAMDEBUG=1 OPAM_ROOT=$(OPAM_ROOT) PATH=$(BIN):$(PATH)
OPAM = $(ENV) opam --yes --root $(OPAM_ROOT)
CHECK = $(ENV) opam-check --root $(OPAM_ROOT)

BINARIES_ = $(BINARIES:%=$(BIN)/%)
BUILDS    = $(foreach bin, $(BINARIES), ../_obuild/$(bin)/$(bin).asm)
ARCHIVES  = $(PACKAGES:%=packages/%.tar.gz)

.PHONY: all rsync server git

all: rsync server git
	@

run:
	$(MAKE) fresh && \
	$(MAKE) binaries && \
	$(MAKE) init && \
	$(MAKE) list && \
	$(MAKE) upload && \
	$(MAKE) install-remove && \
	$(MAKE) list && \
	$(MAKE) install-opt && \
	$(MAKE) list && \
	$(MAKE) install && \
	$(MAKE) list && \
	$(MAKE) upload-new && \
	$(MAKE) list && \
	$(MAKE) upgrade && \
	$(MAKE) list && \
	$(MAKE) downgrade && \
	$(MAKE) list && \
	$(MAKE) switch-alias && \
	$(MAKE) list && \
	$(MAKE) switch-env-packages

rsync:
	$(MAKE) REPOKIND=rsync run

server:
	$(MAKE) fresh
	$(MAKE) binaries
	rm -rf $(OPAM_SERVER_ROOT)
	($(ENV) opam-server --root $(OPAM_SERVER_ROOT) & \
	(sleep 1 && $(MAKE) REPOKIND=server OPAM_REPO=$(OPAM_SERVER) run \
	  && killall opam-server || killall opam-server))

git:
	$(MAKE) REPOKIND=git run

$(BINARIES_): $(BUILDS)
	mkdir -p $(BIN)
	for bin in $(BINARIES); do \
	  cp ../_obuild/$$bin/$$bin.asm $(BIN)/$$bin ; \
	done

binaries: $(BINARIES_)

init:
	rm -rf $(OPAM_REPO)
	mkdir -p $(OPAM_REPO)
	mkdir $(OPAM_REPO)/opam
	mkdir $(OPAM_REPO)/descr
	mkdir $(OPAM_REPO)/compilers
ifeq ($(REPOKIND), git)
	mkdir $(OPAM_REPO)/url
	cd $(OPAM_REPO) && git init && \
	  touch README && git add README && \
	  git commit -a -m "Initial commit"
endif
	$(OPAM) init $(REPO) $(OPAM_REPO) -kind $(REPOKIND)

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
	cp packages/P5.opam       $(OPAM_REPO)/opam/P5.1.opam
	cp packages/P5/README     $(OPAM_REPO)/descr/P5.1
	echo '$(OPAM_GIT)/P5'   > $(OPAM_REPO)/url/P5.1
	cd $(OPAM_REPO) && git add * && git commit -a -m "Adding P5"
	cp compilers/*            $(OPAM_REPO)/compilers/
	cd $(OPAM_REPO)/compilers && git add * && git commit -a -m "Adding compilers"
	rm -rf $(OPAM_GIT) && mkdir -p $(OPAM_GIT)
	mkdir $(OPAM_GIT)/P1-1 && cp packages/P1-1/* $(OPAM_GIT)/P1-1/
	mkdir $(OPAM_GIT)/P2   && cp packages/P2/*   $(OPAM_GIT)/P2/
	mkdir $(OPAM_GIT)/P3   && cp packages/P3/*   $(OPAM_GIT)/P3/
	mkdir $(OPAM_GIT)/P4   && cp packages/P4/*   $(OPAM_GIT)/P4/
	mkdir $(OPAM_GIT)/P5   && cp packages/P5/*   $(OPAM_GIT)/P5/
	cd $(OPAM_GIT)/P1-1 && git init && git add * && git commit -a -m "initial commit"
	cd $(OPAM_GIT)/P2   && git init && git add * && git commit -a -m "initial commit"
	cd $(OPAM_GIT)/P3   && git init && git add * && git commit -a -m "initial commit"
	cd $(OPAM_GIT)/P4   && git init && git add * && git commit -a -m "initial commit"
	cd $(OPAM_GIT)/P5   && git init && git add * && git commit -a -m "initial commit"
else
	cd packages && \
	$(OPAM) upload -opam P1-1.opam -descr P1-1/README -archive P1-1.tar.gz -repo $(REPO)
	cd packages && \
	$(OPAM) upload -opam P2.opam -descr P2/README -archive P2.tar.gz -repo $(REPO)
	cd packages && \
	$(OPAM) upload -opam P3.opam -descr P3/README -archive P3.tar.gz -repo $(REPO)
	cd packages && \
	$(OPAM) upload -opam P4-1.opam -descr P4/README -archive P4.tar.gz -repo $(REPO)
	cd packages && \
	$(OPAM) upload -opam P5.opam -descr P5/README -archive P5.tar.gz -repo $(REPO)
endif
	cp compilers/* $(OPAM_REPO)/compilers/
	$(OPAM) update # update the list of available packages with the one being updated

list:
	$(OPAM) list

install-remove:
	$(OPAM) install P1 &&\
	$(CHECK) -l install-remove P1.1 &&\
	$(OPAM) remove P1

install-opt:
	$(OPAM) install P5 &&\
	$(CHECK) -l install-opt P1.1 P5.1 &&\
	$(OPAM) install P2 &&\
	$(CHECK) -l install-opt P1.1 P2.1 P5.1 &&\
	$(OPAM) remove P5 &&\
	$(OPAM) remove P2 &&\
	$(OPAM) remove P1 &&\
	$(CHECK) -l install-opt

install:
	$(OPAM) install P1 &&\
	$(CHECK) -l install P1.1 &&\
	$(OPAM) install P2 &&\
	$(CHECK) -l install P1.1 P2.1 &&\
	$(OPAM) install P3 &&\
	$(CHECK) -l install P1.1 P2.1 P3.1~weird-version.test &&\
	$(OPAM) install P4 &&\
	$(CHECK) -l install P1.1 P2.1 P3.1~weird-version.test P4.1

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
ifeq ($(REPOKIND), git)
	$(CHECK) -l upgrade P1.1 P2.1 P3.1~weird-version.test P4.3
else
	$(CHECK) -l upgrade P1.2 P2.1 P3.1~weird-version.test P4.3
endif

downgrade:
	$(OPAM) install P4.2
	$(CHECK) -l downgrade P1.1 P2.1 P3.1~weird-version.test P4.2

switch-alias:
	$(OPAM) remove P3.1~weird-version.test P4.2
	$(CHECK) -l switch-alias P1.1 P2.1
	$(OPAM) switch -alias test $(OCAMLVERSION) -clone
	$(CHECK) -l switch-alias P1.1 P2.1
	$(OPAM) switch -list

switch-env-packages:
	$(OPAM) switch dummy
ifeq ($(REPOKIND), git)
	$(CHECK) -l switch-env-packages P1.1 P2.1 P3.1~weird-version.test P4.3
else
	$(CHECK) -l switch-env-packages P1.2 P2.1 P3.1~weird-version.test P4.3
endif
	./test-TEST.sh $(OPAM_ROOT)/dummy/build/P4.3/P4.env "1"

switch:
	$(MAKE) fresh
	$(MAKE) binaries
	$(MAKE) init
	$(MAKE) upload
	$(MAKE) install
	$(OPAM) switch -list
	$(OPAM) switch -clone 3.12.1-byte

packages/%.tar.gz: packages/% packages/%/*
	cd packages && tar cz $* > $*.tar.gz

clean:
	rm -f $(ARCHIVES)

fresh:
	rm -rf $(OPAM_ROOT) $(OPAM_REPO) $(BIN)
