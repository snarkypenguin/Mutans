#TEX =  pdflatex
TEX =  xelatex
chapters = chap1.tex chap2.tex chap3.tex chap4.tex chap4.tex concl.tex
periphera = manifest.tex thesis.tex prelude.tex newcom.tex biby.tex thesis.bib index.tex 


all:	thesis.pdf

check:
	chktex -n1 -n4 -n13 -n36 -n39 -n44  thesis.tex > warnings || true

thesis.pdf: ${chapters} ${periphera}
	$(TEX) -halt-on-error -recorder thesis
	bibtex thesis
	bibtex thesis
	$(TEX) -halt-on-error -recorder thesis
	$(TEX) -halt-on-error -recorder thesis
#	say Finished.

clean:
	rm -f *.log *.aux *.bbl *.blg *.idx thesis.pdf
