Notes de la séance 2 de TransProg M2
====================================

Fopix est le langage maison qui nous servira de point de départ. 

### Le cours 2 : Visite guidée de Fopix ###

 - [l'AST de Fopix](../src/main/scala/trac/fopix/FopixAST.scala) et ses constructions

 - la syntaxe concrète : voir [exemples](../examples), ou les [PrettyPrint](../src/main/scala/trac/fopix/FopixPP.scala) et [Parser](../src/main/scala/trac/fopix/FopixParser.scala) fournis

 - caractéristiques principales :
   - des fonctions récursives générales, pas d'application partielle
   - un poil de fonctionnel : la tête d'un appel de fonction est à calculer (cf. `Call`)
   - dans un premier temps vous pourrez vous concentrer sur le cas simple des `Call(Fun(f),args)`
   - `Op` : opérations binaires de base (arithmétique et comparaisons)
   - `Prim` : quelques autres opérations primitives fournies
   - En particulier, parmi ces primitives, des opérations sur des tableaux impératifs : `mkNew(size)`, `mkGet(array,index)`, `mkSet(array,index,value)`

### TP2 ###

 - Prise en main du compilateur `trac`, de son architecture, de ses options
 - Commencer à compléter [FopixInterp](../src/main/scala/trac/fopix/FopixInterp.scala), l'interpréteur de Fopix
 - Quels sont les choix de *sémantique* à effectuer pour cet interpréteur ? Proposer votre choix favori et me demander pour confirmation.

NB: Pour valider vos choix de sémantique, j'ai mis en ligne un petit [interpréteur Fopix](http://www.irif.fr/~letouzey/fopix-interp) de référence (merci scalajs).
