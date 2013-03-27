PREFIX=/usr/local/

ocp-build: primitives ocp-build.boot win32_c.c
	awk '{ print "external "$$1": unit -> unit = \"" $$1 "\"" }' primitives > primitives.ml
	ocamlc -o ocp-build.run -custom -make-runtime win32_c.c primitives.ml unix.cma  -cclib -lunix
	cat ocp-build.run ocp-build.boot > ocp-build
	chmod +x ocp-build
	./ocp-build -help >/dev/null 2>&1

install: ocp-build
	cp ocp-build $(PREFIX)/bin/ocp-build

clean:
	rm -f *~ ocp-build ocp-build.run primitives.cm? win32_c.o win32_c.obj primitives.ml

