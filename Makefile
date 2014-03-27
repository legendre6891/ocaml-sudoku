#
# Makefile for CS 11 Ocaml track lab 3.
#

default: fast

slow:
	ocamlc -o sudoku /usr/lib/ocaml/bigarray.mli sudoku.ml

fast:
	ocamlopt -o sudoku bigarray.cmxa sudoku.ml

clean:
	rm -f *.cmo *.cmi *.cmx *.o sudoku solutions

test:
	./run_test





