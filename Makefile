RESULT=test
all:
	ocamlc -a foncteur.ml -o foncteur.cma
	ocamlc -a projet.ml -o projet.cma
	ocamlc -a graphics.cma display.ml -o display.cma
	ocamlc -a test.ml -o test.cma
	ocamlc display.cma foncteur.cma projet.cma test.ml -o test.exe
	ocamlc display.cma foncteur.cma projet.cma recti.ml -o recti.exe
clean:
	rm -rf *.cma *.cmo test.exe