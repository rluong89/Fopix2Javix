Notes de la séance 6 de TransProg M2
====================================

## Conversion CPS : introduction

Une *continuation* est un argument fonctionnel qu'une fonction reçoit comme paramètre, et qu'elle va lancer sur son résultat au lieu de faire un simple `return` sur ce réultat (ou équivalent selon les langages).

La *CPS* (continuation passing style), ou en français "style par passage de continuation", correspond à l'usage systématique de continuations tout au long d'un programme.

La mise en forme CPS (ou conversion CPS) est la transformation de programme qui prend un programme quelconque et en fait une version CPS. L'intérêt de cette transformation est d'obtenir un programme ne faisant plus que des appels de fonctions *terminaux*.
En particulier, les fonctions récursives seront toutes récursives terminales (*tailrec*) après la mise en CPS, ce qui nous permettra une compilation optimisée.

Voir [CpsDemo.scala](CpsDemo.scala).

## Les nouveaux langages intermédiaires : Anfix et Kontix

#### Anfix

Anfix correspond à une forme dite *ANF* chez d'autres auteurs (pour *Administrative Normal Form*). L'idée est de forcer l'usage de variables locales entre toutes opérations, par exemple un `f(x+1)` sera transformé en `let y = x+1 in f(y)`.
Cette étape préparatoire simplifiera le passage à Kontix et à ses formes CPS.

Voir [AnfixAST.scala](../src/main/scala/trac/anfix/AnfixAST.scala).

#### Kontix

Kontix correspond à du code mis en CPS (on y fait donc des "Kontinuations").

Voir [KontixAST.scala](../src/main/scala/trac/kontix/KontixAST.scala).


#### Les deux chemins de compilation

```
              directe
Fopix ------------------>  Javix
  \                        /
   \ ajout let            /  compil optimisée (pas d'adresse de retour)
    \                    /
      Anfix ----------> Kontix
              cps
```
