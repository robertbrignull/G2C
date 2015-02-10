all: g2c.pdf

g2c.pdf: g2c.tex
	rm *.aux *.log
	pdflatex -halt-on-error g2c.tex

#####

clean:
	rm *.pdf *.aux *.log
