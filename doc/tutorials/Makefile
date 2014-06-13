all: opam.wiki
	cd opam.wiki && git pull
	$(MAKE) Advanced_Install.pdf Basic_Usage.pdf \
	        Advanced_Usage.pdf Basic_Usage.pdf \
	        Packaging.pdf Developing.pdf

opam.wiki:
	git clone git://github.com/ocaml/opam.wiki.git

%.pdf: opam.wiki/%.md
	pandoc $^ -o $@

clean:
	@[ -d opam.wiki ] && \
	if cd opam.wiki && git diff-index --quiet origin/master; then \
	  cd .. && rm -rf opam.wiki; \
	else echo "WARNING: doc/tutorials/opam.wiki has local changes, not removing"; \
	fi || true
