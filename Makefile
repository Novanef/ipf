RESULT=test
all:
	ocamlc -a foncteur.ml -o foncteur.cma
	ocamlc -a projet.ml -o projet.cma
	ocamlc -a test.ml -o test.cma
	ocamlc projet.cma foncteur.cma test.ml -o test.exe
clean:
	rm -rf *.cma *.cmo test.exe