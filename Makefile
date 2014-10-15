all: g2c

G2C = exceptions.cmo AST.ml printing.cmo lexer.cmo parser.cmo infer_types.cmo main.cmo
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
infer_types.cmi: AST.cmi exceptions.cmi
infer_types.cmo: AST.cmi exceptions.cmi infer_types.cmi
infer_types.cmx: AST.cmi exceptions.cmi infer_types.cmi
lexer.cmi: exceptions.cmi parser.cmi
lexer.cmo: exceptions.cmi parser.cmi lexer.cmi
lexer.cmx: exceptions.cmi parser.cmx lexer.cmi
main.cmo: exceptions.cmi parser.cmi lexer.cmi infer_types.cmi printing.cmi
main.cmx: exceptions.cmi parser.cmx lexer.cmx infer_types.cmi printing.cmi
parser.cmi: exceptions.cmi AST.cmi
parser.cmo: exceptions.cmi AST.cmi parser.cmi
parser.cmx: exceptions.cmi AST.cmi parser.cmi
printing.cmi: AST.cmi
printing.cmo: AST.cmi printing.cmi
printing.cmx: AST.cmi printing.cmi
