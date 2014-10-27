all: g2c

G2C = exceptions.cmo AST_0_U.ml AST_1_F.ml AST_2_K.ml AST_3_C.ml common.cmo printing_1_F.cmo printing_2_K.cmo printing_3_C.cmo lexer.cmo parser.cmo trans_0_infer_types.cmo trans_1_F_to_K.cmo trans_2_K_to_C.cmo opt_1_unique_ids.cmo opt_2_K.cmo main.cmo
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

AST_0_U.cmi:
AST_1_F.cmi:
AST_2_K.cmi:
AST_3_C.cmi:
common.cmi:
common.cmo: common.cmi
common.cmx: common.cmi
exceptions.cmi:
exceptions.cmo: exceptions.cmi
exceptions.cmx: exceptions.cmi
trans_0_infer_types.cmi: AST_0_U.cmi AST_1_F.cmi exceptions.cmi
trans_0_infer_types.cmo: AST_0_U.cmi AST_1_F.cmi exceptions.cmi trans_0_infer_types.cmi
trans_0_infer_types.cmx: AST_0_U.cmi AST_1_F.cmi exceptions.cmi trans_0_infer_types.cmi
trans_1_F_to_K.cmi: AST_1_F.cmi AST_2_K.cmi common.cmi exceptions.cmi
trans_1_F_to_K.cmo: AST_1_F.cmi AST_2_K.cmi common.cmi exceptions.cmi trans_1_F_to_K.cmi
trans_1_F_to_K.cmx: AST_1_F.cmi AST_2_K.cmi common.cmi exceptions.cmi trans_1_F_to_K.cmi
trans_2_K_to_C.cmi: AST_2_K.cmi AST_3_C.cmi exceptions.cmi
trans_2_K_to_C.cmo: AST_2_K.cmi AST_3_C.cmi exceptions.cmi trans_2_K_to_C.cmi
trans_2_K_to_C.cmx: AST_2_K.cmi AST_3_C.cmi exceptions.cmi trans_2_K_to_C.cmi
lexer.cmi: exceptions.cmi parser.cmi
lexer.cmo: exceptions.cmi parser.cmi lexer.cmi
lexer.cmx: exceptions.cmi parser.cmx lexer.cmi
main.cmo: exceptions.cmi parser.cmi lexer.cmi trans_0_infer_types.cmi trans_1_F_to_K.cmi trans_2_K_to_C.cmi printing_1_F.cmi printing_2_K.cmi printing_3_C.cmi
main.cmx: exceptions.cmi parser.cmx lexer.cmx trans_0_infer_types.cmi trans_1_F_to_K.cmi trans_2_K_to_C.cmi printing_1_F.cmi opt_2_K.cmi printing_2_K.cmi printing_3_C.cmi
opt_1_unique_ids.cmi: AST_1_F.cmi common.cmi
opt_1_unique_ids.cmo: AST_1_F.cmi common.cmi opt_1_unique_ids.cmi
opt_1_unique_ids.cmx: AST_1_F.cmi common.cmi opt_1_unique_ids.cmi
opt_2_K.cmi: AST_2_K.cmi
opt_2_K.cmo: AST_2_K.cmi opt_2_K.cmi
opt_2_K.cmx: AST_2_K.cmi opt_2_K.cmi
parser.cmi: exceptions.cmi AST_0_U.cmi
parser.cmo: exceptions.cmi AST_0_U.cmi parser.cmi
parser.cmx: exceptions.cmi AST_0_U.cmi parser.cmi
printing_1_F.cmi: AST_1_F.cmi
printing_1_F.cmo: AST_1_F.cmi printing_1_F.cmi
printing_1_F.cmx: AST_1_F.cmi printing_1_F.cmi
printing_2_K.cmi: AST_2_K.cmi
printing_2_K.cmo: AST_2_K.cmi printing_2_K.cmi
printing_2_K.cmx: AST_2_K.cmi printing_2_K.cmi
printing_3_C.cmi: AST_3_C.cmi
printing_3_C.cmo: AST_3_C.cmi printing_3_C.cmi
printing_3_C.cmx: AST_3_C.cmi printing_3_C.cmi
