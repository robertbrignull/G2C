all: g2c.pdf

g2c.pdf: g2c.tex images/*
	pdflatex -halt-on-error g2c.tex

#####

clean:
	rm -f *.pdf *.aux *.log
