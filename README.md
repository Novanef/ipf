# Projet ipf Timothé Rios

---

## Utilisation

* `make` permet de créer les executables **test_recti.exe** et **test_eucli.exe**
* `make clean` supprime tous les fichiers créés par `make`
* **test_recti.exe**  et **test_eucli.exe** lisent sur l'entrée standard un entier sur une ligne, correspondant au nombre de points à relier, puis pour chaque point, son abscisse et son ordonée sur une ligne séparés par un espace
* si jamais on veut réduire le nombre d'arbres candidats crées, on peut diminuer l'entier situé à l'avant dernière ligne de **recti.ml** ou **eucli.ml**
