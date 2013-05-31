.PHONY: all
all: dev-manual.pdf
html: dev-manual.html

%.pdf: %.tex
	pdflatex $*
	pdflatex $*

%.html: %.tex
	hevea htmlmacros.hva $<

clean:
	rm -f *~ *.log *.toc *.out *.bbl *.blg *.aux
