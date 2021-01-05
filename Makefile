RESULT=test
all:
	ocamlc -g -a foncteur.ml -o foncteur.cma
	ocamlc -g -a projet.ml -o projet.cma
	ocamlc -g -a graphics.cma display.ml -o display.cma
	ocamlc -g -a test.ml -o test.cma
	ocamlc -g display.cma foncteur.cma projet.cma test.ml -o test.exe
	ocamlc -g display.cma foncteur.cma projet.cma recti.ml -o recti.exe
	ocamlc -g display.cma foncteur.cma projet.cma eucli.ml -o eucli.exe
clean:
	rm -rf *.cma *.cmo test.exe