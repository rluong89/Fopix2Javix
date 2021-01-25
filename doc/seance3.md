Notes de la séance 3 de TransProg M2
====================================

## Suite du travail sur FopixInterp

NB: Pour valider vos choix de sémantique, j'ai mis en ligne un petit [interpréteur Fopix](http://www.irif.fr/~letouzey/fopix-interp) de référence (merci scalajs).

## Javix : Une idéalisation d'un fragment de la JVM

Visite guidée:

 - [l'AST de Javix](../src/main/scala/trac/javix/JavixAST.scala) et ses constructions.

 - Le [pretty-printer de Javix](../src/main/scala/trac/javix/JavixPP.scala) fabrique pour vous des fichiers `.j` à destination de `jasmin`.

 - Un [interpréteur pas-à-pas](../src/main/scala/trac/javix/JavixInterp.scala) fourni pour aider à la mise-au-point (cf. l'option `-trace` de Trap).

Session de démo dans la console Scala:

```scala
import trac._
import IntOp._
import javix.AST._
val codejavix = List(Push(6),Push(7),IOp(Add),IPrint,Return)
val progjavix = Program("Test",codejavix,100,100)
javix.PP.pp(progjavix)
Trac.writeFile("Test.j",javix.PP.pp(progjavix))
javix.Interp.eval(progjavix)
javix.Interp.eval(progjavix,true)
```

## Fopix2Javix : Compilation directe de Fopix vers Javix

Première tâche pour le projet : écrire une traduction directe de Fopix vers Javix.
A débuter dès aujourd'hui avec moi (TP3), au moins pour les parties faciles.

#### Validateur JVM

Il est *normal* que le code Javix produit par Fopix2Javix soit
fréquemment refusé par le validateur de la JVM, vu que notre usage
de la pile ne sera pas forcément prévisible à l'avance.
Du coup pour l'instant: `java -noverify FooBar` 
  
#### Arrêt

l'exécution de votre code Javix doit terminer par un `Return`.
Il n'y a donc pas de réponse finale, par contre vos exemples
contiendront sans doute des affichages (`IPrint` et `SPrint`).

#### La suite au prochain numéro

Les autres détails de la traduction directe entre Fopix et Javix seront vus
en [séance 4](seance4.md) (convention d'appel de fonction, boxing, etc).
