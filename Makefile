# This should point to an installation of prob c
PROBC = /home/robert/Documents/oxford/year\ 4/project/languages/prob-c/

EXAMPLES =\
  examples/bayesian\
  examples/large-bayesian\
  examples/coin-flip\
  examples/gaussian-unknown-mean\
  examples/tricky-coin\
  examples/sum-equals\
  examples/function-test\
  examples/marsaglia\
  examples/influence-diagram\
  examples/list-test\
  examples/mem-test\
  examples/neural-net\
  examples/kalman-filter-application\
  examples/kalman-filter-missing-values\
  examples/kalman-filter-learning\
  examples/fib\
  examples/observe-removal-test
  


all: g2c $(EXAMPLES)

G2C = src/exceptions.ml src/exceptions.mli src/AST_U.ml src/AST_U.mli src/AST_F.ml src/AST_F.mli src/AST_K.ml src/AST_K.mli src/AST_K_Prime.ml src/AST_K_Prime.mli src/AST_H.ml src/AST_H.mli src/AST_C.ml src/AST_C.mli src/common.ml src/common.mli src/printing_U.ml src/printing_U.mli src/printing_F.ml src/printing_F.mli src/printing_K.ml src/printing_K.mli src/printing_H.ml src/printing_H.mli src/printing_C.ml src/printing_C.mli src/c_headers/default.c src/c_headers/lists.c src/c_headers/discrete.c src/c_headers/categorical.c src/lexer.mll src/parser.mly src/trans_infer_types.ml src/trans_infer_types.mli src/trans_F_to_K.ml src/trans_F_to_K.mli src/trans_K_to_H.ml src/trans_K_to_H.mli src/trans_H_to_C.ml src/trans_H_to_C.mli src/opt_unique_ids.ml src/opt_unique_ids.mli src/opt_K.ml src/opt_K.mli src/main.ml
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
