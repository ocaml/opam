TMP_DIR   = $(realpath .)/tmp
OPAM_ROOT = $(TMP_DIR)/OPAM.ROOT
OPAM_REPO = $(TMP_DIR)/OPAM.REPO

# repositoy name
REPO      = test
REPOKIND ?= local

# To test GIT repo
OPAM_GIT = $(TMP_DIR)/OPAM.GIT

PACKAGES  = P1-0 P1-1 P1-2 P2 P3 P4 P5

ifndef OPAM
  OPAM = $(realpath ../src/opam)
endif
ENV   = PATH=$(PATH) $(DEBUG) OPAMKEEPBUILDDIR=1 OPAMROOT=$(OPAM_ROOT) OPAMSWITCH= OPAMNOBASEPACKAGES=1 OPAMYES=1 OPAM=$(OPAM)
OPAMBIN  = $(ENV) $(OPAM)
ifndef CHECK
  CHECK = $(ENV) $(realpath ../src/opam-check)
endif

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

printf = /usr/bin/printf

define RUN
@COUNT=$$(ls -1 $(REPOKIND)-*.log 2>/dev/null | wc -l); \
LOG=$$($(printf) "$(REPOKIND)-%02d-$(1).log" $$COUNT); \
$(printf) "  %02d \e[1m%-20s\e[m ..................................... " \
  $$COUNT $(1); \
if $(MAKE) $(1) >$$LOG 2>&1; then \
  $(printf) "\e[32m[ OK ]\e[m\n"; \
else \
  $(printf) "\e[31m$(1) FAILED\e[m\n\n" >>$$LOG; \
  $(printf) "\e[31m[FAIL]\e[m\n"; \
  { $(printf) "\e[31m>> %s FAILED <<\e[m\n" $(1); cat $$LOG; } \
	>> failed-$(REPOKIND).log; \
fi; \
cat $$LOG >> fulltest-$(REPOKIND).log
endef

run:
	@rm -f failed-$(REPOKIND).log fulltest-$(REPOKIND).log
	@rm -f $(REPOKIND)-*.log
	$(call RUN,init)
	$(call RUN,upload)
	$(call RUN,install-remove)
	$(call RUN,list)
	$(call RUN,install-opt)
	$(call RUN,list)
	$(call RUN,install)
	$(call RUN,list)
	$(call RUN,reinstall)
	$(call RUN,list)
	$(call RUN,upload-new)
	$(call RUN,list)
	$(call RUN,upgrade)
	$(call RUN,list)
	$(call RUN,downgrade)
	$(call RUN,list)
	$(call RUN,switch-alias)
	$(call RUN,list)
	$(call RUN,switch-env-packages)
	$(call RUN,repo)
	$(call RUN,list)
	@if [ -e failed-$(REPOKIND).log ]; \
	then echo "FAILED! Logs in `pwd`/failed-$(REPOKIND).log"; exit 1; \
	else echo "SUCCESS!"; fi

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
	$(OPAMBIN) init --bare --no-setup $(REPO) $(OPAM_REPO) -k $(REPOKIND)

define mkurl
  echo 'src: "http://dev.null" checksum: "'`openssl md5 packages/$(2) |cut -d' ' -f2`'"' \
  > $(OPAM_REPO)/packages/$(1)/url
endef

upload: $(ARCHIVES)
	cp -r packages/ocaml $(OPAM_REPO)/packages
	mkdir -p $(OPAM_REPO)/packages/P1.0
	cp packages/P1-0.opam     $(OPAM_REPO)/packages/P1.0/opam
	$(call mkurl,P1.0,P1-0.tar.gz)
	mkdir -p $(OPAM_REPO)/packages/P1.1
	cp packages/P1-1.opam     $(OPAM_REPO)/packages/P1.1/opam
	cp packages/P1-1/README   $(OPAM_REPO)/packages/P1.1/descr
	$(call mkurl,P1.1,P1-1.tar.gz)
	mkdir -p $(OPAM_REPO)/packages/P2.1
	cp packages/P2/README     $(OPAM_REPO)/packages/P2.1/descr
	cp packages/P2.opam       $(OPAM_REPO)/packages/P2.1/opam
	$(call mkurl,P2.1,P2.tar.gz)
	mkdir -p $(OPAM_REPO)/packages/P3.1~weird-version.test
	cp packages/P3.opam       $(OPAM_REPO)/packages/P3.1~weird-version.test/opam
	cp packages/P3/README     $(OPAM_REPO)/packages/P3.1~weird-version.test/descr
	$(call mkurl,P3.1~weird-version.test,P3.tar.gz)
	mkdir -p $(OPAM_REPO)/packages/P4.1
	cp packages/P4-1.opam     $(OPAM_REPO)/packages/P4.1/opam
	cp packages/P4/README     $(OPAM_REPO)/packages/P4.1/descr
	$(call mkurl,P4.1,P4.tar.gz)
	mkdir -p $(OPAM_REPO)/packages/P5.1
	cp packages/P5.opam       $(OPAM_REPO)/packages/P5.1/opam
	cp packages/P5/README     $(OPAM_REPO)/packages/P5.1/descr
	$(call mkurl,P5.1,P5.tar.gz)
ifeq ($(REPOKIND), git)
	cd $(OPAM_REPO)/packages/ocaml.system && git add * && git commit -a -m "Adding ocaml.system"
	cd $(OPAM_REPO)/packages/ocaml.20 && git add * && git commit -a -m "Adding ocaml.20"
	cd $(OPAM_REPO)/packages/ocaml.10+a+b && git add * && git commit -a -m "Adding ocaml.10+a+b"
	echo 'git: "$(OPAM_GIT)/P1-0"' > $(OPAM_REPO)/packages/P1.0/url
	cd $(OPAM_REPO)/packages/P1.0/ && git add * && git commit -a -m "Adding P0"
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
	rm -rf $(OPAM_GIT) && mkdir -p $(OPAM_GIT)
	mkdir $(OPAM_GIT)/P1-0 && cp packages/P1-0/* $(OPAM_GIT)/P1-0/
	mkdir $(OPAM_GIT)/P1-1 && cp packages/P1-1/* $(OPAM_GIT)/P1-1/
	mkdir $(OPAM_GIT)/P2   && cp packages/P2/*   $(OPAM_GIT)/P2/
	mkdir $(OPAM_GIT)/P3   && cp packages/P3/*   $(OPAM_GIT)/P3/
	mkdir $(OPAM_GIT)/P4   && cp packages/P4/*   $(OPAM_GIT)/P4/
	mkdir $(OPAM_GIT)/P5   && cp packages/P5/*   $(OPAM_GIT)/P5/
	cd $(OPAM_GIT)/P1-0 && git init && git add * && git commit -a -m "initial commit"
	cd $(OPAM_GIT)/P1-1 && git init && git add * && git commit -a -m "initial commit"
	cd $(OPAM_GIT)/P2   && git init && git add * && git commit -a -m "initial commit"
	cd $(OPAM_GIT)/P3   && git init && git add * && git commit -a -m "initial commit"
	cd $(OPAM_GIT)/P4   && git init && git add * && git commit -a -m "initial commit"
	cd $(OPAM_GIT)/P5   && git init && git add * && git commit -a -m "initial commit"
else
	mkdir -p $(OPAM_REPO)/cache
	for p in P1-0 P1-1 P1-2 P2 P3 P4 P5; do \
	  f=packages/$$p.tar.gz; \
	  md5=`openssl md5 $$f |cut -d' ' -f2`; \
	  dir=$(OPAM_REPO)/cache/md5/`echo $$md5 |head -c2`; \
	  mkdir -p $$dir; \
	  cp $$f $$dir/$$md5; \
	done
	echo 'archive-mirrors: "$(OPAM_REPO)/cache"' >> $(OPAM_REPO)/repo
endif
	$(OPAMBIN) update
	$(OPAMBIN) switch create system --packages ocaml.system

list:
	$(OPAMBIN) list -A

install-remove:
	$(CHECK) -l install-remove-1 ocaml.system
	$(OPAMBIN) install P1
	$(CHECK) -l install-remove-2 ocaml.system P1.1
	$(OPAMBIN) remove P1
	$(CHECK) -l install-remove-3 ocaml.system

install-opt:
	$(CHECK) -l install-opt-1 ocaml.system
	$(OPAMBIN) install P5
	test -f $(OPAM_ROOT)/system/lib/p5/p2_absent
	$(CHECK) -l install-opt-2 ocaml.system P1.1 P5.1
	$(OPAMBIN) remove P5
	$(CHECK) -l install-opt-3 ocaml.system P1.1
	$(OPAMBIN) install P5
	$(CHECK) -l install-opt-4 ocaml.system P1.1 P5.1
	$(OPAMBIN) remove P5 -a
	$(CHECK) -l install-opt-5 ocaml.system
	$(OPAMBIN) install P5
	$(CHECK) -l install-opt-6 ocaml.system P1.1 P5.1
	$(OPAMBIN) install P2
	test -f $(OPAM_ROOT)/system/lib/p5/p2_present
	$(CHECK) -l install-opt-7 ocaml.system P1.1 P2.1 P5.1
	$(OPAMBIN) remove P5 -a
	$(CHECK) -l install-opt-8 ocaml.system P1.1 P2.1
	$(OPAMBIN) remove P2 -a
	$(CHECK) -l install-opt-9 ocaml.system
	$(OPAMBIN) install P1 P2 P5
	test -f $(OPAM_ROOT)/system/lib/p5/p2_present
	$(CHECK) -l install-opt-10 ocaml.system P1.1 P2.1 P5.1
	$(OPAMBIN) remove P2 -a
	test -f $(OPAM_ROOT)/system/lib/p5/p2_absent
	$(CHECK) -l install-opt-11 ocaml.system P1.1 P5.1
	$(OPAMBIN) remove P1
	$(CHECK) -l install-opt-12 ocaml.system

install:
	$(CHECK) -l install-1 ocaml.system
	$(OPAMBIN) install P1
	$(CHECK) -l install-2 ocaml.system P1.1
	$(OPAMBIN) install P2
	$(CHECK) -l install-3 ocaml.system P1.1 P2.1
	$(OPAMBIN) install P3
	$(CHECK) -l install-4 ocaml.system P1.1 P2.1 P3.1~weird-version.test
	$(OPAMBIN) install P4
	$(CHECK) -l install-5 ocaml.system P1.1 P2.1 P3.1~weird-version.test P4.1

reinstall:
	$(CHECK) -l reinstall-1 ocaml.system P1.1 P2.1 P3.1~weird-version.test P4.1
	$(OPAMBIN) reinstall P1
	$(CHECK) -l reinstall-2 ocaml.system P1.1 P2.1 P3.1~weird-version.test P4.1

upload-new:
	mkdir $(OPAM_REPO)/packages/P4.2
	cp packages/P4-2.opam   $(OPAM_REPO)/packages/P4.2/opam
	cp packages/P4/README   $(OPAM_REPO)/packages/P4.2/descr
	$(call mkurl,P4.2,P4.tar.gz)
	mkdir $(OPAM_REPO)/packages/P4.3
	cp packages/P4-3.opam   $(OPAM_REPO)/packages/P4.3/opam
	cp packages/P4/README   $(OPAM_REPO)/packages/P4.3/descr
	$(call mkurl,P4.3,P4.tar.gz)
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
	$(call mkurl,P1.2,P1-2.tar.gz)
endif
	$(OPAMBIN) update

upgrade:
	$(CHECK) -l upgrade-1 ocaml.system P1.1 P2.1 P3.1~weird-version.test P4.1
	eval `$(OPAMBIN) config env`; [ "X$$P1" = "Xversion1" ]
	$(OPAMBIN) upgrade
ifeq ($(REPOKIND), git)
	$(CHECK) -l upgrade-2 ocaml.system P1.1 P2.1 P3.1~weird-version.test P4.3
else
	$(CHECK) -l upgrade-2 ocaml.system P1.2 P2.1 P3.1~weird-version.test P4.3
	eval `$(OPAMBIN) config env`; [ "X$$P1" = "Xversion2" ]
endif

downgrade:
ifeq ($(REPOKIND), git)
	$(CHECK) -l downgrade-1 ocaml.system P1.1 P2.1 P3.1~weird-version.test P4.3
else
	$(CHECK) -l downgrade-1 ocaml.system P1.2 P2.1 P3.1~weird-version.test P4.3
endif
	$(OPAMBIN) install P4.2
	$(CHECK) -l downgrade-2 ocaml.system P1.1 P2.1 P3.1~weird-version.test P4.2

switch-alias:
	$(CHECK) -l switch-alias-1 ocaml.system P1.1 P2.1 P3.1~weird-version.test P4.2
	$(OPAMBIN) remove P3.1~weird-version.test P4.2
	$(CHECK) -l switch-alias-2 ocaml.system P1.1 P2.1
	$(OPAMBIN) switch export $(TMP_DIR)/export
	$(OPAMBIN) switch create test system
	$(CHECK) -l switch-alias-3 ocaml.system
	$(OPAMBIN)	switch import $(TMP_DIR)/export
	$(CHECK) -l switch-alias-4 ocaml.system P1.1 P2.1
	$(OPAMBIN) switch create test2 20
	$(CHECK) -l switch-alias-5 ocaml.20
	$(OPAMBIN) install P1
	$(CHECK) -l switch-alias-6 ocaml.20 P1.1
	$(OPAMBIN) switch system
	$(CHECK) -l switch-alias-7 ocaml.system P1.1 P2.1
	$(OPAMBIN) switch remove test test2

switch-env-packages:
	$(CHECK) -l switch-env-packages-1 ocaml.system P1.1 P2.1
	$(OPAMBIN) switch install 10+a+b --packages=ocaml.10+a+b,P1,P2,P3,P4
	$(CHECK) -l switch-env-packages-2 ocaml.10+a+b P1.1 P2.1 P3.1~weird-version.test P4.3
	./test-TEST.sh $(wildcard $(OPAM_ROOT)/10+a+b/build/P4.3/P4*.env) "1"

repo:
	$(OPAMBIN) repo add $(REPO)2 $(OPAM_REPO) -k $(REPOKIND)
	$(OPAMBIN) repo remove $(REPO)2
	$(OPAMBIN) repo remove $(REPO)

packages/%.tar.gz: packages/%
	cd packages && tar czf $*.tar.gz $*

clean:
	rm -f test.log fulltest.log
	rm -f $(ARCHIVES)
	rm -rf $(TMP_DIR)
