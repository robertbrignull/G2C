all: g2c

G2C = exceptions.cmo AST_U.ml AST_F.ml AST_K.ml AST_K_Prime.ml AST_H.ml AST_C.ml common.cmo printing_U.cmo printing_F.cmo printing_K.cmo printing_H.cmo printing_C.cmo lexer.cmo parser.cmo trans_infer_types.cmo trans_F_to_K.cmo trans_K_to_H.cmo trans_H_to_C.cmo opt_unique_ids.cmo opt_K.cmo main.cmo
g2c: $(G2C)
	ocamlc -o ../g2c $(G2C)

parser.mli parser.ml: parser.mly
	ocamlyacc parser.mly

lexer.ml: lexer.mll
	ocamllex lexer.mll

clean:
	rm -f *cmx *cma *.cmo *.cmi
	rm -f lexer.ml
	rm -f parser.ml parser.mli

%.cmi : %.mli
	ocamlc -c $(INCLUDE) $<

%.cmo : %.ml
	ocamlc -c $(INCLUDE) $<

###

AST_U.cmi:
AST_F.cmi:
AST_K.cmi:
AST_K_Prime.cmi:
AST_C.cmi:
common.cmi:
common.cmo: common.cmi
common.cmx: common.cmi
exceptions.cmi:
exceptions.cmo: exceptions.cmi
exceptions.cmx: exceptions.cmi
trans_infer_types.cmi: AST_U.cmi AST_F.cmi printing_U.cmi exceptions.cmi
trans_infer_types.cmo: AST_U.cmi AST_F.cmi printing_U.cmi exceptions.cmi trans_infer_types.cmi
trans_infer_types.cmx: AST_U.cmi AST_F.cmi printing_U.cmi exceptions.cmi trans_infer_types.cmi
trans_F_to_K.cmi: AST_F.cmi AST_K.cmi common.cmi exceptions.cmi
trans_F_to_K.cmo: AST_F.cmi AST_K.cmi common.cmi exceptions.cmi trans_F_to_K.cmi
trans_F_to_K.cmx: AST_F.cmi AST_K.cmi common.cmi exceptions.cmi trans_F_to_K.cmi
trans_K_to_H.cmi: AST_K.cmi AST_H.cmi common.cmi exceptions.cmi
trans_K_to_H.cmo: AST_K.cmi AST_H.cmi common.cmi exceptions.cmi trans_K_to_H.cmi
trans_K_to_H.cmx: AST_K.cmi AST_H.cmi common.cmi exceptions.cmi trans_K_to_H.cmi
trans_H_to_C.cmi: AST_H.cmi AST_C.cmi common.cmi exceptions.cmi
trans_H_to_C.cmo: AST_H.cmi AST_C.cmi common.cmi exceptions.cmi trans_H_to_C.cmi
trans_H_to_C.cmx: AST_H.cmi AST_C.cmi common.cmi exceptions.cmi trans_H_to_C.cmi
lexer.cmi: exceptions.cmi parser.cmi
lexer.cmo: exceptions.cmi parser.cmi lexer.cmi
lexer.cmx: exceptions.cmi parser.cmx lexer.cmi
main.cmo: exceptions.cmi parser.cmi lexer.cmi trans_infer_types.cmi trans_F_to_K.cmi trans_K_to_H.cmi trans_H_to_C.cmi printing_F.cmi printing_K.cmi printing_H.cmi printing_C.cmi opt_unique_ids.cmi opt_K.cmi
main.cmx: exceptions.cmi parser.cmi lexer.cmi trans_infer_types.cmi trans_F_to_K.cmi trans_K_to_H.cmi trans_H_to_C.cmi printing_F.cmi printing_K.cmi printing_H.cmi printing_C.cmi opt_unique_ids.cmi opt_K.cmi
opt_unique_ids.cmi: AST_F.cmi common.cmi
opt_unique_ids.cmo: AST_F.cmi common.cmi opt_unique_ids.cmi
opt_unique_ids.cmx: AST_F.cmi common.cmi opt_unique_ids.cmi
opt_K.cmi: AST_K.cmi AST_K_Prime.cmi
opt_K.cmo: AST_K.cmi AST_K_Prime.cmi opt_K.cmi
opt_K.cmx: AST_K.cmi AST_K_Prime.cmi opt_K.cmi
parser.cmi: exceptions.cmi AST_U.cmi
parser.cmo: exceptions.cmi AST_U.cmi parser.cmi
parser.cmx: exceptions.cmi AST_U.cmi parser.cmi
printing_U.cmi: AST_U.cmi common.cmi
printing_U.cmo: AST_U.cmi common.cmi printing_U.cmi
printing_U.cmx: AST_U.cmi common.cmi printing_U.cmi
printing_F.cmi: AST_F.cmi common.cmi
printing_F.cmo: AST_F.cmi common.cmi printing_F.cmi
printing_F.cmx: AST_F.cmi common.cmi printing_F.cmi
printing_K.cmi: AST_K.cmi common.cmi
printing_K.cmo: AST_K.cmi common.cmi printing_K.cmi
printing_K.cmx: AST_K.cmi common.cmi printing_K.cmi
printing_H.cmi: AST_H.cmi common.cmi
printing_H.cmo: AST_H.cmi common.cmi printing_H.cmi
printing_H.cmx: AST_H.cmi common.cmi printing_H.cmi
printing_C.cmi: AST_C.cmi common.cmi c_headers/default.c c_headers/lists.c c_headers/discrete.c c_headers/categorical.c
printing_C.cmo: AST_C.cmi common.cmi c_headers/default.c c_headers/lists.c c_headers/discrete.c c_headers/categorical.c printing_C.cmi
printing_C.cmx: AST_C.cmi common.cmi c_headers/default.c c_headers/lists.c c_headers/discrete.c c_headers/categorical.c printing_C.cmi
