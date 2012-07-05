.PHONY: all
all: roadmap.pdf

%.pdf: %.tex
	pdflatex $*
	pdflatex $*

clean:
	rm -f *~ *.log *.toc *.out *.bbl *.blg *.aux