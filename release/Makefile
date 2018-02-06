TAG = master
VERSION = $(shell git describe $(TAG))
OPAM_VERSION = $(subst -,~,$(VERSION))
GIT_URL = ..

FULL_ARCHIVE_URL = https://github.com/ocaml/opam/releases/download/$(VERSION)/opam-full-$(VERSION).tar.gz

TARGETS = x86_64-linux i686-linux armhf-linux arm64-linux
# todo: x86_64-darwin

OCAMLV = 4.04.2
# currently hardcoded in Dockerfile.in
OCAML_URL = https://caml.inria.fr/pub/distrib/ocaml-$(basename $(OCAMLV))/ocaml-$(OCAMLV).tar.gz

HOST_OS = $(shell uname -s | tr A-Z a-z)
HOST = $(shell uname -m)-$(HOST_OS)

all: $(patsubst %,out/opam-$(VERSION)-%,$(TARGETS))

out/opam-full-$(VERSION).tar.gz:
	mkdir -p out
	cd out && curl -OfL $(FULL_ARCHIVE_URL) || { \
	  git clone $(GIT_URL) -b $(TAG) --depth 1 opam-full-$(VERSION); \
	  sed -i 's/^AC_INIT(opam,.*)/AC_INIT(opam,$(OPAM_VERSION))/' \
	    opam-full-$(VERSION)/configure.ac; \
	  $(MAKE) -C opam-full-$(VERSION) configure download-ext; \
	  tar cz --exclude-vcs opam-full-$(VERSION) -f $(notdir $@); \
	  rm -rf opam-full-$(VERSION); \
	}

build/Dockerfile.x86_64-linux: Dockerfile.in
	mkdir -p build && sed 's/%TARGET_TAG%/amd64-jessie/g' $^ | sed 's/%CONF%//g' >$@
build/Dockerfile.i686-linux: Dockerfile.in
	mkdir -p build && sed 's/%TARGET_TAG%/i386-jessie/g' $^ | sed 's/%CONF%/-host i686-linux/g' >$@
build/Dockerfile.armhf-linux: Dockerfile.in
	mkdir -p build && sed 's/%TARGET_TAG%/armhf-jessie/g' $^ | sed 's/%CONF%//g' >$@
build/Dockerfile.arm64-linux: Dockerfile.in
	mkdir -p build && sed 's/%TARGET_TAG%/arm64-jessie/g' $^ | sed 's/%CONF%//g' >$@


build/%.image: build/Dockerfile.%
	docker build -t opam-build-$* -f $^ build
	touch $@

# Actually, this is for debian 8 jessie, and varies wildly
CLINKING_linux = \
-Wl,-Bstatic \
-lunix -lbigarray -lmccs_stubs -lmccs_glpk_stubs \
-lstdc++ \
-Wl,-Bdynamic \
-static-libgcc

CLINKING_darwin = \
-lunix -lbigarray -lmccs_stubs -lmccs_glpk_stubs \
-lstdc++

CLINKING_openbsd = $(CLINKING_darwin)

LINKING = (-noautolink $(patsubst %,-cclib %,$(CLINKING_$(1))))

EXPORTS_openbsd = \
CPATH=/usr/local/include: \
LIBRARY_PATH=/usr/local/lib: \


%: opam-$(VERSION)-%

opam-$(VERSION)-%: out/opam-$(VERSION)-%
	ln -sf $^ $@

# host: opam-$(VERSION)-$(HOST)

# Build for the local host. Containerised builds, below, are preferred, but not always available
build/$(HOST).env:
	mkdir -p build/$(HOST)
	cd build/$(HOST) && curl -OL $(OCAML_URL)
	cd build/$(HOST) && tar xzf ocaml-$(OCAMLV).tar.gz
	cd build/$(HOST)/ocaml-$(OCAMLV) && \
	  ./configure -prefix $(shell pwd)/build/$(HOST) && \
	  $(MAKE) world opt.opt && \
	  $(MAKE) install
	rm -rf build/$(HOST)/ocaml-$(OCAMLV) build/$(HOST)/ocaml-$(OCAMLV).tar.gz
	touch $@

# Actually builds out/opam-$(VERSION)-$(HOST), but we don't want to override the
# rule that goes through a container
host: out/opam-full-$(VERSION).tar.gz build/$(HOST).env
	rm -rf build/opam-full-$(VERSION)
	cd build && tar xzf ../$<
	( export \
	    PATH=$(shell pwd)/build/$(HOST)/bin:$$PATH \
	    MAKE=$(MAKE) \
	    $(EXPORTS_$(HOST_OS)); \
	  cd build/opam-full-$(VERSION) && \
	  ./configure && \
	  echo "$(call LINKING,$(HOST_OS))" >src/client/linking.sexp && \
	  $(MAKE) lib-ext JBUILDER_ARGS="--root=`pwd`"; \
	  $(MAKE) opam JBUILDER_ARGS="--root=`pwd`"; \
	)
	cp build/opam-full-$(VERSION)/opam out/opam-$(VERSION)-$(HOST)
	strip out/opam-$(VERSION)-$(HOST)
	rm -rf build/opam-full-$(VERSION)

# Containerised builds
out/opam-$(VERSION)-%-linux: build/%-linux.image out/opam-full-$(VERSION).tar.gz
	docker run --rm -v `pwd`/out:/src \
	  -e "VERSION=$(VERSION)" \
	  -e "TARGET=$*-linux" \
	  -e "LINKING=$(call LINKING,$(HOST_OS))" \
	  opam-build-$*-linux

clean:
	rm -rf build

distclean: clean
	rm -rf out

REMOTE_DIR = /tmp/opam-release
REMOTE_MAKE = make
remote: out/opam-full-$(VERSION).tar.gz
	ssh "$(REMOTE)" "mkdir -p $(REMOTE_DIR)/out"
	scp Makefile "$(REMOTE):$(REMOTE_DIR)/"
	scp "$^" "$(REMOTE):$(REMOTE_DIR)/$^"
	ssh "$(REMOTE)" 'sh -c "cd $(REMOTE_DIR) && $(REMOTE_MAKE) host TAG=$(TAG) VERSION=$(VERSION) OCAMLV=$(OCAMLV)"'
	scp "$(REMOTE):$(REMOTE_DIR)/out/opam-$(VERSION)*" out/
