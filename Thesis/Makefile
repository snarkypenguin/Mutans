
chapters = chap1.tex chap2.tex chap3.tex chap4.tex chap4.tex concl.tex
periphera = manifest.tex thesis.tex prelude.tex newcom.tex biby.tex thesis.bib index.tex 


all:	thesis.pdf

thesis.pdf: ${chapters} ${periphera}
	pdflatex -halt-on-error -recorder thesis
	bibtex thesis
	bibtex thesis
	pdflatex -halt-on-error -recorder thesis
	pdflatex -halt-on-error -recorder thesis
#	say Finished.

clean:
	rm -f *.log *.aux *.bbl *.blg *.idx thesis.pdf
