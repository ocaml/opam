all: opam.wiki
	cd opam.wiki && git pull
	$(MAKE) Packaging.pdf Tutorial.pdf Developing.pdf

opam.wiki:
	git clone git://github.com/OCamlPro/opam.wiki.git

%.pdf: opam.wiki/%.md
	pandoc $^ -o $@