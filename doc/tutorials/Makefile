all: opam.wiki
	cd opam.wiki && git pull
	$(MAKE) Advanced_Install.pdf Basic_Usage.pdf \
	        Advanced_Usage.pdf Basic_Usage.pdf \
	        Packaging.pdf Developing.pdf

opam.wiki:
	git clone git://github.com/OCamlPro/opam.wiki.git

%.pdf: opam.wiki/%.md
	pandoc $^ -o $@