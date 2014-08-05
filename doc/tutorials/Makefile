all: opam.wiki
	cd opam.wiki && git pull
	$(MAKE) Quick_Install.html Basic_Usage.html FAQ.html \
		Advanced_Usage.html Packaging.html \
	        Specifying_Solver_Preferences.html

opam.wiki:
	git clone git://github.com/ocaml/opam.wiki.git

%.html: opam.wiki/%.md
	omd $^ -o $@

clean:
	@[ -d opam.wiki ] && \
	if cd opam.wiki && git diff-index --quiet origin/master; then \
	  cd .. && rm -rf opam.wiki; \
	else echo "WARNING: doc/tutorials/opam.wiki has local changes, not removing"; \
	fi || true
