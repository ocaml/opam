TMP_DIR   = $(realpath .)/tmp
OPAM_ROOT = $(TMP_DIR)/OPAM.ROOT
OPAM_REPO = $(TMP_DIR)/OPAM.REPO

# repositoy name
REPO      = test
REPOKIND ?= local

# To test GIT repo
OPAM_GIT = $(TMP_DIR)/OPAM.GIT

PACKAGES  = P1-1 P1-2 P2 P3 P4 P5

_OPAM = $(realpath ../_obuild/opam/opam.asm)
ENV   = PATH=$(PATH) $(DEBUG) OPAMKEEPBUILDDIR=1 OPAMROOT=$(OPAM_ROOT) OPAMYES=1 OPAM=$(_OPAM)
OPAM  = $(ENV) $(_OPAM)
CHECK = $(ENV) $(realpath ../_obuild/opam-check/opam-check.asm)

ifeq ($(OPAMTESTQUIET), 1)
  DEBUG =
else
  DEBUG = OPAMDEBUG=2 OCAMLRUNPARAM=b
endif

ARCHIVES  = $(PACKAGES:%=packages/%.tar.gz)

.PHONY: all local git

all: local git
	@

quiet:
	$(MAKE) OPAMTESTQUIET=1 all

run:
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
	@echo "SUCCESS!"

local:
	$(MAKE) clean
	$(MAKE) REPOKIND=local run

git:
	$(MAKE) clean
	$(MAKE) REPOKIND=git run

init:
	rm -rf $(OPAM_REPO)
	mkdir -p $(OPAM_REPO)
ifeq ($(REPOKIND), git)
	cd $(OPAM_REPO) && git init && \
	  touch README && git add README && \
	  git commit -a -m "Initial commit"
endif
	$(OPAM) init --no-setup --no-base-packages $(REPO) $(OPAM_REPO) -k $(REPOKIND)

upload: $(ARCHIVES)
ifeq ($(REPOKIND), git)
	mkdir -p $(OPAM_REPO)/packages/P1.1
	cp packages/P1-1.opam     $(OPAM_REPO)/packages/P1.1/opam
	cp packages/P1-1/README   $(OPAM_REPO)/packages/P1.1/descr
	echo 'git: "$(OPAM_GIT)/P1-1"' > $(OPAM_REPO)/packages/P1.1/url
	cd $(OPAM_REPO) && git add * && git commit -a -m "Adding P1"
	mkdir -p $(OPAM_REPO)/packages/P2.1
	cp packages/P2/README     $(OPAM_REPO)/packages/P2.1/descr
	cp packages/P2.opam       $(OPAM_REPO)/packages/P2.1/opam
	echo 'git: "$(OPAM_GIT)/P2"'   > $(OPAM_REPO)/packages/P2.1/url
	cd $(OPAM_REPO) && git add * && git commit -a -m "Adding P2"
	mkdir -p $(OPAM_REPO)/packages/P3.1~weird-version.test
	cp packages/P3.opam       $(OPAM_REPO)/packages/P3.1~weird-version.test/opam
	cp packages/P3/README     $(OPAM_REPO)/packages/P3.1~weird-version.test/descr
	echo 'git: "$(OPAM_GIT)/P3"'   > $(OPAM_REPO)/packages/P3.1~weird-version.test/url
	cd $(OPAM_REPO) && git add * && git commit -a -m "Adding P3"
	mkdir -p $(OPAM_REPO)/packages/P4.1
	cp packages/P4-1.opam     $(OPAM_REPO)/packages/P4.1/opam
	cp packages/P4/README     $(OPAM_REPO)/packages/P4.1/descr
	echo 'git: "$(OPAM_GIT)/P4"'   > $(OPAM_REPO)/packages/P4.1/url
	cd $(OPAM_REPO) && git add * && git commit -a -m "Adding P4"
	mkdir -p $(OPAM_REPO)/packages/P5.1
	cp packages/P5.opam       $(OPAM_REPO)/packages/P5.1/opam
	cp packages/P5/README     $(OPAM_REPO)/packages/P5.1/descr
	echo 'git: "$(OPAM_GIT)/P5"'   > $(OPAM_REPO)/packages/P5.1/url
	cd $(OPAM_REPO) && git add * && git commit -a -m "Adding P5"
	mkdir -p $(OPAM_REPO)/compilers
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
	$(OPAM) upload --opam P1-1.opam --descr P1-1/README --archive P1-1.tar.gz $(REPO)
	cd packages && \
	$(OPAM) upload --opam P2.opam --descr P2/README --archive P2.tar.gz $(REPO)
	cd packages && \
	$(OPAM) upload --opam P3.opam --descr P3/README --archive P3.tar.gz $(REPO)
	cd packages && \
	$(OPAM) upload --opam P4-1.opam --descr P4/README --archive P4.tar.gz $(REPO)
	cd packages && \
	$(OPAM) upload --opam P5.opam --descr P5/README --archive P5.tar.gz $(REPO)
endif
	mkdir -p $(OPAM_REPO)/compilers
	cp compilers/* $(OPAM_REPO)/compilers/
	$(OPAM) update # update the list of available packages with the one being updated

list:
	$(OPAM) list

install-remove:
	$(CHECK) -l install-remove-1
	$(OPAM) install P1
	$(CHECK) -l install-remove-2 P1.1
	$(OPAM) remove P1
	$(CHECK) -l install-remove-3

install-opt:
	$(CHECK) -l install-opt-1
	$(OPAM) install P5
	$(CHECK) -l install-opt-2 P1.1 P5.1
	$(OPAM) remove P5
	$(CHECK) -l install-opt-3 P1.1
	$(OPAM) install P5
	$(CHECK) -l install-opt-4 P1.1 P5.1
	$(OPAM) remove P5 -a
	$(CHECK) -l install-opt-5
	$(OPAM) install P5
	$(CHECK) -l install-opt-6 P1.1 P5.1
	$(OPAM) install P2
	$(CHECK) -l install-opt-7 P1.1 P2.1 P5.1
	$(OPAM) remove P5 -a
	$(CHECK) -l install-opt-8 P1.1 P2.1
	$(OPAM) remove P2 -a
	$(CHECK) -l install-opt-9

install:
	$(CHECK) -l install-1
	$(OPAM) install P1
	$(CHECK) -l install-2 P1.1
	$(OPAM) install P2
	$(CHECK) -l install-3 P1.1 P2.1
	$(OPAM) install P3
	$(CHECK) -l install-4 P1.1 P2.1 P3.1~weird-version.test
	$(OPAM) install P4
	$(CHECK) -l install-5 P1.1 P2.1 P3.1~weird-version.test P4.1

upload-new:
ifeq ($(REPOKIND), git)
	echo "(* new line *)" >> $(OPAM_GIT)/P1-1/p1.ml
	cd $(OPAM_GIT)/P1-1 && git commit -a -m "a small change"
	mkdir $(OPAM_REPO)/packages/P4.2
	cp packages/P4-2.opam   $(OPAM_REPO)/packages/P4.2/opam
	cp packages/P4/README   $(OPAM_REPO)/packages/P4.2/descr
	echo 'git: "$(OPAM_GIT)/P4"' > $(OPAM_REPO)/packages/P4.2/url
	mkdir $(OPAM_REPO)/packages/P4.3
	cp packages/P4-3.opam   $(OPAM_REPO)/packages/P4.3/opam
	cp packages/P4/README   $(OPAM_REPO)/packages/P4.3/descr
	echo 'git: "$(OPAM_GIT)/P4"' > $(OPAM_REPO)/packages/P4.3/url
	cd $(OPAM_REPO) && git add * && git commit -a -m "Adding P4.2 and P4.3"
else
	cd packages && $(OPAM) upload --opam P1-2.opam --descr P1-2/README --archive P1-2.tar.gz $(REPO)
	cd packages && $(OPAM) upload --opam P4-2.opam --descr P4/README --archive P4.tar.gz $(REPO)
	cd packages && $(OPAM) upload --opam P4-3.opam --descr P4/README --archive P4.tar.gz $(REPO)
endif
	$(OPAM) update

upgrade:
	$(CHECK) -l upgrade-1 P1.1 P2.1 P3.1~weird-version.test P4.1
	$(OPAM) upgrade
ifeq ($(REPOKIND), git)
	$(CHECK) -l upgrade-2 P1.1 P2.1 P3.1~weird-version.test P4.3
else
	$(CHECK) -l upgrade-2 P1.2 P2.1 P3.1~weird-version.test P4.3
endif

downgrade:
ifeq ($(REPOKIND), git)
	$(CHECK) -l downgrade-1 P1.1 P2.1 P3.1~weird-version.test P4.3
else
	$(CHECK) -l downgrade-1 P1.2 P2.1 P3.1~weird-version.test P4.3
endif
	$(OPAM) install P4.2
	$(CHECK) -l downgrade-2 P1.1 P2.1 P3.1~weird-version.test P4.2

switch-alias:
	$(CHECK) -l switch-alias-1 P1.1 P2.1 P3.1~weird-version.test P4.2
	$(OPAM) remove P3.1~weird-version.test P4.2
	$(CHECK) -l switch-alias-2 P1.1 P2.1
	$(OPAM) switch export -f $(TMP_DIR)/export
	$(OPAM) switch install test --alias-of system --no-base-packages
	$(CHECK) -l switch-alias-3
	$(OPAM)	switch import -f $(TMP_DIR)/export
	$(CHECK) -l switch-alias-4 P1.1 P2.1
	$(OPAM) switch install test2 --alias-of 20
	$(CHECK) -l switch-alias-5
	$(OPAM) install P1
	$(CHECK) -l switch-alias-6 P1.1
	$(OPAM) switch system
	$(CHECK) -l switch-alias-7 P1.1 P2.1
	$(OPAM) switch remove test test2

switch-env-packages:
	$(CHECK) -l switch-env-packages-1 P1.1 P2.1
	$(OPAM) switch 10
ifeq ($(REPOKIND), git)
	$(CHECK) -l switch-env-packages-2 P1.1 P2.1 P3.1~weird-version.test P4.3
else
	$(CHECK) -l switch-env-packages-2 P1.2 P2.1 P3.1~weird-version.test P4.3
endif
	./test-TEST.sh $(wildcard $(OPAM_ROOT)/10/build/P4.3/P4*.env) "1"

packages/%.tar.gz: packages/% 
	cd packages && tar czf $*.tar.gz $* 

clean:
	rm -f $(ARCHIVES)
	rm -rf $(TMP_DIR)
