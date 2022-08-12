TAG = master
VERSION = $(shell git describe $(TAG))
OPAM_VERSION = $(subst -,~,$(VERSION))
GIT_URL = ..

OCAMLV = 4.13.1
# currently hardcoded in Dockerfile.in
OCAML_URL = https://caml.inria.fr/pub/distrib/ocaml-$(basename $(OCAMLV))/ocaml-$(OCAMLV).tar.gz

HOST_OS = $(shell uname -s | tr A-Z a-z | sed 's/darwin/macos/')
HOST = $(shell uname -m | sed 's/amd64/x86_64/')-$(HOST_OS)
OUTDIR = out/$(TAG)

# The equivalent of "which <cmd>". Taken from the GNU Make documentation
pathsearch = $(firstword $(wildcard $(addsuffix /$(1),$(subst :, ,$(PATH)))))

x86_64-linux: $(OUTDIR)/opam-$(VERSION)-x86_64-linux
i686-linux: $(OUTDIR)/opam-$(VERSION)-i686-linux
armhf-linux: $(OUTDIR)/opam-$(VERSION)-armhf-linux
arm64-linux: $(OUTDIR)/opam-$(VERSION)-arm64-linux

$(OUTDIR)/opam-full-$(VERSION).tar.gz:
	mkdir -p "$(OUTDIR)"
	git clone $(GIT_URL) -b $(TAG) "$(OUTDIR)/opam-full-$(VERSION)"
	sed 's/^AC_INIT(opam,.*)/AC_INIT(opam,$(OPAM_VERSION))/' "$(OUTDIR)/opam-full-$(VERSION)/configure.ac" > \
	  "$(OUTDIR)/opam-full-$(VERSION)/configure.ac.tmp"
	mv "$(OUTDIR)/opam-full-$(VERSION)/configure.ac.tmp" \
	  "$(OUTDIR)/opam-full-$(VERSION)/configure.ac"
	$(MAKE) -C "$(OUTDIR)/opam-full-$(VERSION)" OCAML=$(call pathsearch,ocaml) configure download-ext
	cd "$(OUTDIR)" && tar cz -f $(notdir $@) --exclude .git/ opam-full-$(VERSION)
	rm -rf "$(OUTDIR)/opam-full-$(VERSION)"

build/Dockerfile.x86_64-linux: Dockerfile.in
	mkdir -p build && sed -e "s/%OCAMLV%/$(OCAMLV)/g" -e 's/%TARGET_TAG%/x86_64-v3.13/g' -e 's/%CONF%//g' $^ >$@
build/Dockerfile.i686-linux: Dockerfile.in
	mkdir -p build && sed -e "s/%OCAMLV%/$(OCAMLV)/g" -e 's/%TARGET_TAG%/x86-v3.13/g' -e 's/%CONF%/-build i586-alpine-linux-musl/g' $^ >$@

# Need to lie about gnueabihf instead of musleabihf, because of a ./configure bug
build/Dockerfile.armhf-linux: Dockerfile.in
	mkdir -p build && sed -e "s/%OCAMLV%/$(OCAMLV)/g" -e 's/%TARGET_TAG%/armv7-v3.13/g' -e 's/%CONF%//g' $^ >$@
build/Dockerfile.arm64-linux: Dockerfile.in
	mkdir -p build && sed -e "s/%OCAMLV%/$(OCAMLV)/g" -e 's/%TARGET_TAG%/arm64-v3.13/g' -e 's/%CONF%//g' $^ >$@


build/%.image: build/Dockerfile.%
	docker build -t opam-build-$* -f $^ build
	touch $@

# Actually, this is for alpine 3.13, and varies
CLINKING_linux = \
-Wl,-Bstatic \
-lunix -lmccs_stubs -lmccs_glpk_stubs -lsha_stubs \
-lstdc++ \
-static-libgcc \
-static
# -Wl,-Bdynamic 

CLINKING_macos = \
-lunix -lmccs_stubs -lmccs_glpk_stubs -lsha_stubs \
-lstdc++

CLINKING_openbsd = $(CLINKING_macos)
CLINKING_freebsd = $(CLINKING_macos)

LINKING = (-noautolink $(patsubst %,-cclib %,$(CLINKING_$(1))))

EXPORTS_openbsd = \
CPATH=/usr/local/include: \
LIBRARY_PATH=/usr/local/lib: \

EXPORTS_freebsd = \
CPATH=/usr/local/include: \
LIBRARY_PATH=/usr/local/lib: \


%: opam-$(VERSION)-%

opam-$(VERSION)-%: $(OUTDIR)/opam-$(VERSION)-%
	ln -sf $^ $@

# host: opam-$(VERSION)-$(HOST)

# Build for the local host. Containerised builds, below, are preferred, but not always available
build/$(HOST).env:
	mkdir -p build/$(HOST)
	cd build/$(HOST) && curl -OL $(OCAML_URL)
	cd build/$(HOST) && tar xzf ocaml-$(OCAMLV).tar.gz
	cd build/$(HOST)/ocaml-$(OCAMLV) && \
	  ./configure --prefix "$(shell pwd)/build/$(HOST)" && \
	  $(MAKE) -j$(JOBS) && \
	  $(MAKE) install
	rm -rf build/$(HOST)/ocaml-$(OCAMLV) build/$(HOST)/ocaml-$(OCAMLV).tar.gz
	touch $@

# Actually builds $(OUTDIR)/opam-$(VERSION)-$(HOST), but we don't want to override the
# rule that goes through a container
host: $(OUTDIR)/opam-full-$(VERSION).tar.gz build/$(HOST).env
	rm -rf build/opam-full-$(VERSION)
	cd build && tar xzf ../$<
	( export \
	    PATH=$(shell pwd)/build/$(HOST)/bin:$$PATH \
	    MAKE=$(MAKE) \
	    $(EXPORTS_$(HOST_OS)); \
	  cd build/opam-full-$(VERSION) && \
	  ./configure && \
	  { $(MAKE) lib-ext || ./configure --with-vendored-deps; } && \
	  echo "$(call LINKING,$(HOST_OS))" >src/client/linking.sexp && \
	  $(MAKE) opam; \
	)
	cp build/opam-full-$(VERSION)/opam $(OUTDIR)/opam-$(VERSION)-$(HOST)
	strip $(OUTDIR)/opam-$(VERSION)-$(HOST)
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
SSH = ssh -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no
SCP = scp -o UserKnownHostsFile=/dev/null -o StrictHostKeyChecking=no
remote: $(OUTDIR)/opam-full-$(VERSION).tar.gz
	$(SSH) "$(REMOTE)" "mkdir -p $(REMOTE_DIR)/${OUTDIR}"
	$(SCP) Makefile "$(REMOTE):$(REMOTE_DIR)/"
	$(SCP) "$^" "$(REMOTE):$(REMOTE_DIR)/$^"
	$(SSH) "$(REMOTE)" 'sh -c "cd $(REMOTE_DIR) && ulimit -s 8192 && $(REMOTE_MAKE) host TAG=$(TAG) VERSION=$(VERSION) OCAMLV=$(OCAMLV)"'
	$(SCP) "$(REMOTE):$(REMOTE_DIR)/$(OUTDIR)/opam-$(VERSION)*" "$(OUTDIR)/"

qemu: $(OUTDIR)/opam-full-$(VERSION).tar.gz
	$(SSH) -p "$(QEMU_PORT)" root@localhost "mkdir -p $(REMOTE_DIR)/${OUTDIR}"
	$(SCP) -P "$(QEMU_PORT)" Makefile "root@localhost:$(REMOTE_DIR)/"
	$(SCP) -P "$(QEMU_PORT)" "$^" "root@localhost:$(REMOTE_DIR)/$^"
	$(SSH) -p "$(QEMU_PORT)" root@localhost 'sh -c "cd $(REMOTE_DIR) && ulimit -s 8192 && $(REMOTE_MAKE) host JOBS=$(JOBS) TAG=$(TAG) VERSION=$(VERSION) OCAMLV=$(OCAMLV)"'
	$(SCP) -P "$(QEMU_PORT)" "root@localhost:$(REMOTE_DIR)/$(OUTDIR)/opam-$(VERSION)*" "$(OUTDIR)/"

macos-local: $(OUTDIR)/opam-full-$(VERSION).tar.gz
	LOCAL_RELEASE_DIR=$(shell mktemp -d); \
          mkdir -p "$${LOCAL_RELEASE_DIR}/${OUTDIR}" && \
          cp Makefile "$^" "$${LOCAL_RELEASE_DIR}" && \
          cd "$${LOCAL_RELEASE_DIR}" && ulimit -s 8192 && arch -arch $(MACOS_ARCH) make host JOBS=$(JOBS) TAG=$(TAG) VERSION=$(VERSION) OCAMLV=$(OCAMLV) && \
          cp $${LOCAL_RELEASE_DIR}/$(OUTDIR)/opam-$(VERSION)* "$(GIT_URL)/release/$(OUTDIR)/"
