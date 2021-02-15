Récapitulatif des tâches pour le projet de TransProg 2021
=========================================================

Date limite de rendu : 19/3/2021

Ce projet comporte deux parties. Terminer d'abord l'interpréteur `FopixInterp` est recommandé, mais pas obligatoire.

## Partie 1 : compilation directe de Fopix vers Javix

Cette partie nécessite de compléter le fichier suivant:

- [Fopix2Javix](../src/main/scala/trac/transl/Fopix2Javix.scala)

Il est normal que les fichiers `.j` obtenus lors de cette partie ne donnent pas des `.class` directement acceptés par le validateur de la JVM. Ils seront donc à lancer via `java -noverify`.

## Partie 2 : compilation par CPS de Fopix vers Javix

Cette partie nécessite de compléter les fichiers suivants:

- [Fopix2Anfix](../src/main/scala/trac/transl/Fopix2Anfix.scala)
- [Anfix2Kontix](../src/main/scala/trac/transl/Anfix2Kontix.scala)
- [Kontix2Javix](../src/main/scala/trac/transl/Kontix2Javix.scala)

Pour les différencier des fichiers obtenus en partie 1, les fichiers produits dans cette partie 2 seront appelés `.k` même s'il s'agit toujours de fichiers destinés à l'assembleur `jasmin`. Cette fois-ci, les `.class` correspondant devront pouvoir être exécutés via `java` (sans le `-noverify`). Attention alors à bien calculer et indiquer la hauteur de pile nécessaire dans votre programme.

## Dépôts git pris en compte pour le rendu de projet

Me contacter si votre dépôt sur le serveur gaufre n'est pas dans la liste

- gudin/transprog-2021
- luong/transprog-2021
- Haifa/transprog-2021
- chaboche/transprog-2021
- thebinome/transprog-2021

