Notes de la séance 1 de TransProg M2
====================================

### Objectif du cours/projet de cette année ###

 Réalisation d'un compilateur à multiples phases implémentant divers transformations de programmes, et en particulier une mise en forme CPS.

 - langage d'entrée: *Fopix*, un langage maison, cf la semaine prochaine

 - langage cible: la *JVM* fournie par Java

 - langage à utiliser pour ce projet : Scala
   - outil de "build" imposé : sbt (simple build tool)
   - editeurs : emacs ou IntelliJ Idea ou vscode ou ...

 - projet à réaliser par groupe de deux au plus

### Généralités / rappels sur interprétation, compilation, machines virtuelles ###

Voir ces [anciens transparents](seance1-generalites.pdf)

### La JVM ###

Voir ces [anciens transparents](seance1-jvm.pdf). En résumé:

 - Présentation des principaux éléments de la Java Virtual Machine:
   - un frame par appel de fonction, avec une pile et une zone de variables locales
   - un tas général (utilisé ici lors d'un `anewarray`)
 - Description de quelques instructions, cf par exemple la [spec de la JVM](http://docs.oracle.com/javase/specs/jvms/se11/html/index.html)
 - Demo du programme assembleur [jasmin](http://jasmin.sourceforge.net/) pour aller d'un fichier `.j` à un `.class`
 - Voir aussi le désassembleur `javap -c`
 - Plus tard, un mini-interprète sera fourni (cf JavixInterp) pour aider au debug.

### TP1 : prise en main de Jasmin ###

Voir [TP1](seance1-jasmin.md).
