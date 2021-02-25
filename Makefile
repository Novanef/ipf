RESULT=test
SRC = ./src
BIN = ./bin
all:
	ocamlc -I $(BIN) -I $(SRC) -g -a $(SRC)/tree.ml -o $(BIN)/tree.cma
	ocamlc -I $(BIN) -I $(SRC) -g -a graphics.cma $(SRC)/display.ml -o $(BIN)/display.cma
	ocamlc -i $(SRC)/tree.ml > $(SRC)/tree.mli
	ocamlc -I $(BIN) -I $(SRC) -g $(BIN)/tree.cma -a $(SRC)/recti.ml -o $(BIN)/recti.cma -I $(SRC)
	ocamlc -I $(BIN) -I $(SRC) -i tree.cma $(SRC)/recti.ml > $(SRC)/recti.mli
	ocamlc -I $(BIN) -I $(SRC) -g $(BIN)/tree.cma $(BIN)/display.cma -a $(SRC)/eucli.ml -o $(BIN)/eucli.cma -I $(SRC)
	ocamlc -I $(BIN) -I $(SRC) -i tree.cma $(SRC)/eucli.ml > $(SRC)/eucli.mli
	ocamlc -I $(BIN) -I $(SRC) -g display.cma tree.cma recti.cma $(SRC)/test_recti.ml -o test_recti.exe 
	ocamlc -I $(BIN) -I $(SRC) -g  tree.cma eucli.cma $(SRC)/test_eucli.ml -o test_eucli.exe -I $(BIN)
	mv $(SRC)/*.cmi $(BIN)
	mv $(SRC)/*.cmo $(BIN)
clean:
	rm -rf $(SRC)/*.cma $(SRC)/*.cmo $(BIN)/*.cma $(BIN)/*.cmo $(SRC)/*.cmi $(BIN)/*.cmi $(SRC)/*.mli test_eucli.exe test_recti.exe