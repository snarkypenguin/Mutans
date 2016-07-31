
chapters = chap1.tex chap2.tex chap3.tex chap4.tex chap4.tex concl.tex app0.tex app1.tex
periphera = thesis.tex prelude.tex newcom.tex biby.tex thesis.bib index.tex 


all:	thesis.pdf

thesis.pdf: thesis.tex prelude.tex newcom.tex chap1.tex  chap2.tex  chap3.tex  chap4.tex 
	pdflatex thesis
	bibtex thesis
	bibtex thesis
	pdflatex thesis
	pdflatex thesis

clean:
	rm -f *.log *.aux *.bbl *.blg *.idx thesis.pdf
