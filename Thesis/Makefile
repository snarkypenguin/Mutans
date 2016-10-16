
chapters = chap1.tex chap2.tex chap3.tex chap4.tex chap4.tex concl.tex tree-ring-formulation.tex
periphera = thesis.tex prelude.tex newcom.tex biby.tex thesis.bib index.tex 


all:	thesis.pdf

thesis.pdf: ${chapters} ${periphera}
	pdflatex thesis
	bibtex thesis
	bibtex thesis
	pdflatex thesis
	pdflatex thesis
#	say Finished.

clean:
	rm -f *.log *.aux *.bbl *.blg *.idx thesis.pdf
