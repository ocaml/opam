.PHONY: all
all: dev-manual.pdf
html: dev-manual.html

%.pdf: %.tex
	pdflatex $*
	pdflatex $*

%.html: %.tex
	hevea -fix htmlmacros.hva $<

%.txt: %.html
	links -dump $< > $@

clean:
	rm -f *~ *.log *toc *.out *.bbl *.blg *aux *.html
