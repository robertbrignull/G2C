all: g2c.pdf

g2c.pdf: g2c.tex g2c.bib images/* clean
	pdflatex -halt-on-error g2c.tex
	bibtex g2c
	pdflatex -halt-on-error g2c.tex
	pdflatex -halt-on-error g2c.tex

#####

clean:
	rm -f *.pdf *.aux *.log *.bbl *.blg
