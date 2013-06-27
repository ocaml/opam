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
	$(MAKE) reinstall && \
	$(MAKE) list && \
	$(MAKE) upload-new && \
	$(MAKE) list && \
	$(MAKE) upgrade && \
	$(MAKE) list && \
	$(MAKE) downgrade && \
	$(MAKE) list && \
	$(MAKE) switch-alias && \
	$(MAKE) list && \
	$(MAKE) switch-env-packages && \
	$(MAKE) repo && \
	$(MAKE) list
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
	mkdir -p $(OPAM_REPO)/packages/P1.1
	cp packages/P1-1.opam     $(OPAM_REPO)/packages/P1.1/opam
	cp packages/P1-1/README   $(OPAM_REPO)/packages/P1.1/descr
	mkdir -p $(OPAM_REPO)/packages/P2.1
	cp packages/P2/README     $(OPAM_REPO)/packages/P2.1/descr
	cp packages/P2.opam       $(OPAM_REPO)/packages/P2.1/opam
	mkdir -p $(OPAM_REPO)/packages/P3.1~weird-version.test
	cp packages/P3.opam       $(OPAM_REPO)/packages/P3.1~weird-version.test/opam
	cp packages/P3/README     $(OPAM_REPO)/packages/P3.1~weird-version.test/descr
	mkdir -p $(OPAM_REPO)/packages/P4.1
	cp packages/P4-1.opam     $(OPAM_REPO)/packages/P4.1/opam
	cp packages/P4/README     $(OPAM_REPO)/packages/P4.1/descr
	mkdir -p $(OPAM_REPO)/packages/P5.1
	cp packages/P5.opam       $(OPAM_REPO)/packages/P5.1/opam
	cp packages/P5/README     $(OPAM_REPO)/packages/P5.1/descr
	mkdir -p $(OPAM_REPO)/compilers
	cp compilers/*            $(OPAM_REPO)/compilers/
ifeq ($(REPOKIND), git)
	echo 'git: "$(OPAM_GIT)/P1-1"' > $(OPAM_REPO)/packages/P1.1/url
	cd $(OPAM_REPO)/packages/P1.1/ && git add * && git commit -a -m "Adding P1"
	echo 'git: "$(OPAM_GIT)/P2"'   > $(OPAM_REPO)/packages/P2.1/url
	cd $(OPAM_REPO)/packages/P2.1/ && git add * && git commit -a -m "Adding P2"
	echo 'git: "$(OPAM_GIT)/P3"'   > $(OPAM_REPO)/packages/P3.1~weird-version.test/url
	cd $(OPAM_REPO)/packages/P3.1~weird-version.test/ && git add * && git commit -a -m "Adding P3"
	echo 'git: "$(OPAM_GIT)/P4"'   > $(OPAM_REPO)/packages/P4.1/url
	cd $(OPAM_REPO)/packages/P4.1/ && git add * && git commit -a -m "Adding P4"
	echo 'git: "$(OPAM_GIT)/P5"'   > $(OPAM_REPO)/packages/P5.1/url
	cd $(OPAM_REPO)/packages/P5.1/ && git add * && git commit -a -m "Adding P5"
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
	mkdir -p $(OPAM_REPO)/archives
	cp packages/P1-1.tar.gz $(OPAM_REPO)/archives/P1.1+opam.tar.gz
	cp packages/P2.tar.gz $(OPAM_REPO)/archives/P2.1+opam.tar.gz
	cp packages/P3.tar.gz $(OPAM_REPO)/archives/P3.1~weird-version.test+opam.tar.gz
	cp packages/P4.tar.gz $(OPAM_REPO)/archives/P4.1+opam.tar.gz
	cp packages/P5.tar.gz $(OPAM_REPO)/archives/P5.1+opam.tar.gz
endif
	$(OPAM) update

list:
	$(OPAM) list -a

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

reinstall:
	$(CHECK) -l reinstall-1 P1.1 P2.1 P3.1~weird-version.test P4.1
	$(OPAM) reinstall P1
	$(CHECK) -l reinstall-2 P1.1 P2.1 P3.1~weird-version.test P4.1

upload-new:
	mkdir $(OPAM_REPO)/packages/P4.2
	cp packages/P4-2.opam   $(OPAM_REPO)/packages/P4.2/opam
	cp packages/P4/README   $(OPAM_REPO)/packages/P4.2/descr
	mkdir $(OPAM_REPO)/packages/P4.3
	cp packages/P4-3.opam   $(OPAM_REPO)/packages/P4.3/opam
	cp packages/P4/README   $(OPAM_REPO)/packages/P4.3/descr
ifeq ($(REPOKIND), git)
	echo "(* new line *)" >> $(OPAM_GIT)/P1-1/p1.ml
	cd $(OPAM_GIT)/P1-1 && git commit -a -m "a small change"
	echo 'git: "$(OPAM_GIT)/P4"' > $(OPAM_REPO)/packages/P4.2/url
	echo 'git: "$(OPAM_GIT)/P4"' > $(OPAM_REPO)/packages/P4.3/url
	cd $(OPAM_REPO) && git add * && git commit -a -m "Adding P4.2 and P4.3"
else
	mkdir $(OPAM_REPO)/packages/P1.2
	cp packages/P1-2.opam   $(OPAM_REPO)/packages/P1.2/opam
	cp packages/P1-2/README $(OPAM_REPO)/packages/P1.2/descr
	cp packages/P1-2.tar.gz $(OPAM_REPO)/archives/P1.2+opam.tar.gz
	cp packages/P4.tar.gz $(OPAM_REPO)/archives/P4.2+opam.tar.gz
	cp packages/P4.tar.gz $(OPAM_REPO)/archives/P4.3+opam.tar.gz
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
	$(OPAM) switch 10+a+b
ifeq ($(REPOKIND), git)
	$(CHECK) -l switch-env-packages-2 P1.1 P2.1 P3.1~weird-version.test P4.3
else
	$(CHECK) -l switch-env-packages-2 P1.2 P2.1 P3.1~weird-version.test P4.3
endif
	./test-TEST.sh $(wildcard $(OPAM_ROOT)/10+a+b/build/P4.3/P4*.env) "1"

repo:
	$(OPAM) repo add $(REPO)2 $(OPAM_REPO) -k $(REPOKIND)
	$(OPAM) repo remove $(REPO)2
	$(OPAM) repo remove $(REPO)

packages/%.tar.gz: packages/% 
	cd packages && tar czf $*.tar.gz $* 

clean:
	rm -f $(ARCHIVES)
	rm -rf $(TMP_DIR)
