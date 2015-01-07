# This should point to an installation of prob c
PROBC = /home/robert/Documents/oxford/year\ 4/project/languages/prob-c/

EXAMPLES = examples/bayesian examples/large-bayesian examples/coin-flip examples/gaussian-unknown-mean examples/tricky-coin examples/sum-equals examples/function-test examples/marsaglia



all: g2c $(EXAMPLES)

G2C = src/exceptions.ml src/exceptions.mli src/AST_0_U.ml src/AST_0_U.mli src/AST_1_F.ml src/AST_1_F.mli src/AST_2_K.ml src/AST_2_K.mli src/AST_3_H.ml src/AST_3_H.mli src/AST_4_C.ml src/AST_4_C.mli src/common.ml src/common.mli src/printing_0_U.ml src/printing_0_U.mli src/printing_1_F.ml src/printing_1_F.mli src/printing_2_K.ml src/printing_2_K.mli src/printing_3_H.ml src/printing_3_H.mli src/printing_4_C.ml src/printing_4_C.mli src/lexer.mll src/parser.mly src/trans_0_infer_types.ml src/trans_0_infer_types.mli src/trans_1_F_to_K.ml src/trans_1_F_to_K.mli src/trans_2_K_to_H.ml src/trans_2_K_to_H.mli src/trans_3_H_to_C.ml src/trans_3_H_to_C.mli src/opt_1_unique_ids.ml src/opt_1_unique_ids.mli src/opt_2_K.ml src/opt_2_K.mli src/main.ml
g2c: $(G2C)
	cd src && $(MAKE)

clean:
	rm -f g2c
	cd src && $(MAKE) clean
	rm -f $(EXAMPLES) $(addsuffix .c,$(EXAMPLES))

###

# For building a test file
%: g2c %.g
	./g2c -i $@.g -o $@.c
	./compile_probc $@.c $@
