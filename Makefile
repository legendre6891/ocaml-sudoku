#
# Makefile for CS 11 Ocaml track lab 3.
#

default: fast

slow:
	ocamlc -o sudoku sudoku.ml

fast:
	ocamlopt.opt -o sudoku bigarray.cmxa sudoku.ml

clean:
	rm -f *.cmo *.cmi *.cmx *.o sudoku solutions

test:
	./run_test





