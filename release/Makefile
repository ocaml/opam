.SHELLFLAGS := -eu$(if $(shell (set -o pipefail 2>/dev/null > /dev/null && echo yes)),o pipefail) $(.SHELLFLAGS)

export SOURCE_DATE_EPOCH=0
export LC_ALL=C

TAG = master
VERSION = $(shell git describe $(TAG))
OPAM_VERSION = $(subst -,~,$(VERSION))
GIT_URL = ..

ALPINE_VERSION = 3.21

HOST_OS = $(shell uname -s | tr A-Z a-z | sed -e 's/_.*//' -e 's/darwin/macos/' -e 's/cygwin/windows/')
HOST = $(shell uname -m | sed 's/amd64/x86_64/')-$(HOST_OS)
OUTDIR = out/$(TAG)

# The equivalent of "which <cmd>". Taken from the GNU Make documentation
pathsearch = $(firstword $(wildcard $(addsuffix /$(1),$(subst :, ,$(PATH)))))

x86_64-linux: $(OUTDIR)/opam-$(VERSION)-x86_64-linux
i686-linux: $(OUTDIR)/opam-$(VERSION)-i686-linux
armhf-linux: $(OUTDIR)/opam-$(VERSION)-armhf-linux
arm64-linux: $(OUTDIR)/opam-$(VERSION)-arm64-linux
ppc64le-linux: $(OUTDIR)/opam-$(VERSION)-ppc64le-linux
s390x-linux: $(OUTDIR)/opam-$(VERSION)-s390x-linux
riscv64-linux: $(OUTDIR)/opam-$(VERSION)-riscv64-linux

$(OUTDIR)/opam-full-$(VERSION).tar.gz:
	mkdir -p "$(OUTDIR)"
	git clone $(GIT_URL) -b $(TAG) "$(OUTDIR)/opam-full-$(VERSION)"
	unset MAKEFLAGS MAKEOVERRIDES && \
	  $(MAKE) -C "$(OUTDIR)/opam-full-$(VERSION)" OCAML=$(call pathsearch,ocaml) download-ext
	( cd "$(OUTDIR)/opam-full-$(VERSION)" && \
	  git archive "$(TAG)" \
		--mtime "$$(git show -s --format=%ct "$(TAG)" | tail -1)" \
		-o "$(abspath $(OUTDIR)/opam-full-$(VERSION).tar)" \
		$$(for f in $$(git ls-files -i -o -x '*'); do echo "--prefix=opam-full-$(VERSION)/$$(dirname -- $$f)/ --add-file=$$f" || exit 1; done) \
		--prefix "opam-full-$(VERSION)/" && \
	  gzip --no-name --best "$(abspath $(OUTDIR)/opam-full-$(VERSION).tar)" )
	rm -rf "$(OUTDIR)/opam-full-$(VERSION)"

build/Dockerfile.x86_64-linux: Dockerfile.in
	mkdir -p build && sed -e 's/%TARGET_TAG%/x86_64-v$(ALPINE_VERSION)/g' $^ >$@
build/Dockerfile.i686-linux: Dockerfile.in
	mkdir -p build && sed -e 's/%TARGET_TAG%/x86-v$(ALPINE_VERSION)/g' $^ >$@

build/Dockerfile.armhf-linux: Dockerfile.in
	mkdir -p build && sed -e 's/%TARGET_TAG%/armhf-v$(ALPINE_VERSION)/g' $^ >$@
build/Dockerfile.arm64-linux: Dockerfile.in
	mkdir -p build && sed -e 's/%TARGET_TAG%/arm64-v$(ALPINE_VERSION)/g' $^ >$@
build/Dockerfile.ppc64le-linux: Dockerfile.in
	mkdir -p build && sed -e 's/%TARGET_TAG%/ppc64le-v$(ALPINE_VERSION)/g' $^ >$@
build/Dockerfile.s390x-linux: Dockerfile.in
	mkdir -p build && sed -e 's/%TARGET_TAG%/s390x-v$(ALPINE_VERSION)/g' $^ >$@
build/Dockerfile.riscv64-linux: Dockerfile.in
	mkdir -p build && sed -e 's/%TARGET_TAG%/riscv64-v$(ALPINE_VERSION)/g' $^ >$@


build/%.image: build/Dockerfile.%
	docker build -t opam-build-$* -f $^ build
	touch $@

CLINKING_linux = \
-Wl,-Bstatic \
-lunix -lmccs_stubs -lmccs_glpk_stubs -lsha_stubs -lopam_core_stubs -lcheckseum_c_stubs \
-lstdc++ \
-static-libgcc \
-static
# -Wl,-Bdynamic

CLINKING_macos = \
-lunix -lmccs_stubs -lmccs_glpk_stubs -lsha_stubs -lopam_core_stubs -lcheckseum_c_stubs \
-lstdc++

CLINKING_freebsd = $(CLINKING_macos)
CLINKING_netbsd = $(CLINKING_macos)

CLINKING_openbsd = \
-lunix -lmccs_stubs -lmccs_glpk_stubs -lsha_stubs -lopam_core_stubs -lcheckseum_c_stubs \
-lstdc++ -lc++ -lc++abi -static

CLINKING_windows = \
-lunix -lmccs_stubs -lmccs_glpk_stubs -lsha_stubs -lopam_core_stubs -lcheckseum_c_stubs \
-l:libstdc++.a -l:libpthread.a \
-Wl,-static \
-ladvapi32 -lgdi32 -luser32 -lshell32 -lole32 -luuid -luserenv

LINKING = (-noautolink $(patsubst %,-cclib %,$(CLINKING_$(1))))

EXPORTS_openbsd = \
CPATH=/usr/local/include: \
LIBRARY_PATH=/usr/local/lib: \

EXPORTS_freebsd = \
CPATH=/usr/local/include: \
LIBRARY_PATH=/usr/local/lib: \

EXPORTS_netbsd = \
CPATH=/usr/local/include: \
LIBRARY_PATH=/usr/local/lib: \

EXTRA_OPAM_CONFIGURE_ARGS_windows = --with-cygwin-setup

%: opam-$(VERSION)-%

opam-$(VERSION)-%: $(OUTDIR)/opam-$(VERSION)-%
	ln -sf $^ $@

# host: opam-$(VERSION)-$(HOST)

# Actually builds $(OUTDIR)/opam-$(VERSION)-$(HOST), but we don't want to override the
# rule that goes through a container
host: $(OUTDIR)/opam-full-$(VERSION).tar.gz
	rm -rf build/opam-full-$(VERSION)
	cd build && tar xzf ../$<
	( export \
	    PATH=$(shell pwd)/build/$(HOST)/bin:$$PATH \
	    MAKE=$(MAKE) \
	    $(EXPORTS_$(HOST_OS)); \
	  cd build/opam-full-$(VERSION) && \
	  unset MAKEFLAGS MAKEOVERRIDES && \
	  make compiler && \
	  ./configure --with-vendored-deps --with-mccs \
	    $(EXTRA_OPAM_CONFIGURE_ARGS_$(HOST_OS)) && \
	  echo "$(call LINKING,$(HOST_OS))" >src/client/linking.sexp && \
	  $(MAKE) opam-stripped; \
	)
	cp build/opam-full-$(VERSION)/opam-stripped $(OUTDIR)/opam-$(VERSION)-$(HOST)
	$(OUTDIR)/opam-$(VERSION)-$(HOST) --version
	rm -rf build/opam-full-$(VERSION)

# Containerised builds
$(OUTDIR)/opam-$(VERSION)-%-linux: $(OUTDIR)/opam-full-$(VERSION).tar.gz build/%-linux.image
	docker run --rm -i \
	  -e "VERSION=$(VERSION)" \
	  -e "TARGET=$*-linux" \
	  -e "LINKING=$(call LINKING,linux)" \
	  opam-build-$*-linux \
	  <$< >$@

clean:
	rm -rf build

distclean: clean
	rm -rf out

REMOTE_DIR = /tmp/opam-release
REMOTE_MAKE = make
REMOTE_SHELL = /bin/sh
SSH = ./ssh-auto.sh password 240 ssh
SCP = ./ssh-auto.sh password 240 scp
remote: $(OUTDIR)/opam-full-$(VERSION).tar.gz
	$(SSH) "$(REMOTE)" "mkdir -p $(REMOTE_DIR)/$(OUTDIR)"
	$(SCP) Makefile "$(REMOTE):$(REMOTE_DIR)/"
	$(SCP) "$^" "$(REMOTE):$(REMOTE_DIR)/$^"
	$(SSH) "$(REMOTE)" 'sh -c "cd $(REMOTE_DIR) && ulimit -s 8192 && $(REMOTE_MAKE) host TAG=$(TAG) VERSION=$(VERSION)"'
	$(SCP) "$(REMOTE):$(REMOTE_DIR)/$(OUTDIR)/opam-$(VERSION)*" "$(OUTDIR)/"

SSH_USER = root
ULIMIT = ulimit -s 8192 &&
qemu: $(OUTDIR)/opam-full-$(VERSION).tar.gz
	$(SSH) -p "$(QEMU_PORT)" $(SSH_USER)@localhost "$(REMOTE_SHELL) -c \"mkdir -p $(REMOTE_DIR)/$(OUTDIR)\""
	$(SCP) -P "$(QEMU_PORT)" Makefile "$(SSH_USER)@localhost:$(REMOTE_DIR)/"
	$(SCP) -P "$(QEMU_PORT)" "$^" "$(SSH_USER)@localhost:$(REMOTE_DIR)/$^"
	$(SSH) -p "$(QEMU_PORT)" $(SSH_USER)@localhost "$(REMOTE_SHELL) -c \"cd $(REMOTE_DIR) && $(ULIMIT) $(REMOTE_MAKE) host JOBS=$(JOBS) TAG=$(TAG) VERSION=$(VERSION)\""
	$(SCP) -P "$(QEMU_PORT)" "$(SSH_USER)@localhost:$(REMOTE_DIR)/$(OUTDIR)/opam-$(VERSION)*" "$(OUTDIR)/"

macos-local: $(OUTDIR)/opam-full-$(VERSION).tar.gz
	LOCAL_RELEASE_DIR=$(shell mktemp -d); \
          mkdir -p "$${LOCAL_RELEASE_DIR}/${OUTDIR}" && \
          cp Makefile "$^" "$${LOCAL_RELEASE_DIR}" && \
          cd "$${LOCAL_RELEASE_DIR}" && ulimit -s 8192 && arch -arch $(MACOS_ARCH) make host JOBS=$(JOBS) TAG=$(TAG) VERSION=$(VERSION) && \
          cp $${LOCAL_RELEASE_DIR}/$(OUTDIR)/opam-$(VERSION)* "$(GIT_URL)/release/$(OUTDIR)/"
