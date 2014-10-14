all: g2c

G2C = exceptions.cmo AST.ml printing.cmo lexer.cmo parser.cmo main.cmo
g2c: $(G2C)
	ocamlc -o g2c $(G2C)

parser.mli parser.ml: parser.mly
	ocamlyacc parser.mly

lexer.ml: lexer.mll
	ocamllex lexer.mll

clean:
	rm -f g2c
	rm -f *cmx *cma *.cmo *.cmi
	rm -f lexer.ml
	rm -f parser.ml parser.mli

%.cmi : %.mli
	ocamlc -c $(INCLUDE) $<

%.cmo : %.ml
	ocamlc -c $(INCLUDE) $<

###

AST.cmi:
exceptions.cmi:
exceptions.cmo: exceptions.cmi
exceptions.cmx: exceptions.cmi
lexer.cmi: exceptions.cmi parser.cmi
lexer.cmo: exceptions.cmi parser.cmi lexer.cmi
lexer.cmx: exceptions.cmi parser.cmx lexer.cmi
main.cmo: exceptions.cmi parser.cmi lexer.cmi printing.cmi
main.cmx: exceptions.cmi parser.cmx lexer.cmx printing.cmi
parser.cmi: exceptions.cmi AST.cmi
parser.cmo: exceptions.cmi AST.cmi parser.cmi
parser.cmx: exceptions.cmi AST.cmi parser.cmi
printing.cmi: AST.cmi
printing.cmo: AST.cmi printing.cmi
printing.cmx: AST.cmi printing.cmi
