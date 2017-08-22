TAG = master
VERSION = $(shell git describe $(TAG) | sed 's/-/~/')
GIT_URL = ..

TARGETS = x86_64-linux i686-linux armhf-linux arm64-linux
# todo: x86_64-darwin

all: $(patsubst %,out/opam-$(VERSION)-%,$(TARGETS))

out/opam-full-$(VERSION).tar.gz:
	mkdir -p out
	git clone $(GIT_URL) -b $(TAG) --depth 1 opam-full-$(VERSION)
	sed -i 's/^AC_INIT(opam,.*)/AC_INIT(opam,'"$(VERSION)"')/' \
	  opam-full-$(VERSION)/configure.ac
	cd opam-full-$(VERSION) && $(MAKE) configure download-ext
	tar cz --exclude-vcs opam-full-$(VERSION) -f $@
	rm -rf opam-full-$(VERSION)

build/Dockerfile.x86_64-linux: Dockerfile.in
	mkdir -p build && sed 's/%TARGET_TAG%/amd64-jessie/g' $^ | sed 's/%CONF%//' >$@
build/Dockerfile.i686-linux: Dockerfile.in
	mkdir -p build && sed 's/%TARGET_TAG%/i386-jessie/g' $^ | sed 's/%CONF%/-host i686-linux/' >$@
build/Dockerfile.armhf-linux: Dockerfile.in
	mkdir -p build && sed 's/%TARGET_TAG%/armhf-jessie/g' $^ | sed 's/%CONF%//' >$@
build/Dockerfile.arm64-linux: Dockerfile.in
	mkdir -p build && sed 's/%TARGET_TAG%/arm64-jessie/g' $^ | sed 's/%CONF%//' >$@

build/%.image: build/Dockerfile.%
	docker build -t opam-build-$* -f $^ build
	touch $@

CLINKING_jessie = \
-Wl,-Bstatic \
-lunix -lbigarray -lmccs_stubs \
-lglpk -lstdc++ -lz -lamd -lcolamd -lltdl \
-Wl,-Bdynamic \
-static-libgcc
LINKING_jessie = -noautolink $(patsubst %,-cclib %,$(CLINKING_jessie))

CLINKING_darwin = \
-lunix -lbigarray -lmccs_stubs \
/usr/local/lib/libglpk.a -lz -lstdc++
LINKING_darwin = -noautolink $(patsubst %,-cclib %,$(CLINKING_darwin))

%: opam-$(VERSION)-%

opam-$(VERSION)-%: out/opam-$(VERSION)-%
	ln -sf $^ $@

out/opam-$(VERSION)-%-linux: build/%-linux.image out/opam-full-$(VERSION).tar.gz
	docker run --rm -v `pwd`/out:/src \
	  -e "VERSION=$(VERSION)" \
	  -e "TARGET=$*-linux" \
	  -e "LINKING=$(LINKING_jessie)" \
	  opam-build-$*-linux

out/opam-$(VERSION)-%-darwin: opam-full-$(VERSION).tar.gz
	@error todo generate appropriate reproducible build env with ocaml 4.04, and fail if not available
	tar xzf $^
	cd opam-full-$(VERSION) && \
	  ./configure --with-mccs && \
	  echo "($(LINKING_darwin))" > src/client/linking.sexp && \
	  make lib-ext opam
	cp opam-full-$(VERSION)/opam opam-$(VERSION)-$*-darwin
	rm -rf opam-full-$(VERSION)

clean:
	rm -rf build

distclean: clean
	rm -rf out
