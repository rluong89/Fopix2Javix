Fopix2Javix : un exemple complet (fact)
=======================================

## En fopix

```
def fact(x) = if x == 0 then 1 else fact(x-1) * x
val _ = print_int(fact(10))
val _ = print_string("\n")
/* answer: 3628800 */
```

Attention, par rapport à [examples/fact.fx](../examples/fact.fx) j'ai
mis la multiplication par `x` à droite du `fact(x-1)`, pas à gauche.
Cela ne change pas le résultat, et cela nous met (artificiellement, ok)
dans une situation où une variable (ici `x`) est encore utilisée après
un sous-appel de fonction, et doit donc être sauvegardé pendant l'appel.


## En Javix

Attention, ce qui suit est un joyeux mélange entre la syntaxe des
fichiers `.j` de jasmin et la syntaxe "idéalisée" de Javix,
p.ex. `Push(n)` au lieu des différents `iconst_0` ou `bispush` ou
`sipush`, etc.

```
;zone principale du programme (main)
;; appel fact(10)
;;; ici environnement vide : pas de variable donc pas de sauvegarde de variable
Push(10)
Box
AStore(0)
Push(1000) ;; code du label retour_000
Goto fact
retour_000:
;;; pas de restauration des variables

;; print_int
Unbox
IPrint
Push(0)
Box

;; val _ = ...
Pop

;; print_string
Ldc("\n")
SPrint
;; optimisation possible : rien derriere
;Push(0)
;Box
;Pop

Return ; fin du main

;zone des fonctions (ici seulement fact)
fact:
;x
ALoad(0)
Unbox
;0
Push(0)
Box
Unbox ;; optimisation : enlever ces les deux dernières lignes 
Ificmp(Ne,else001)
Push(1)
Box
Goto end002:
else001:
;; Ici on doit compiler fact(x-1)*x
;; Compilation de fact(x-1)
;; sauvegarde de x sur la pile
ALoad(0)
;; calcul de l'argument (x-1)
ALoad(0)
Unbox
Push(1)
isub  (= IOP(IntOp.Sub) en Javix)
Box
;; stock l'argument
AStore(0)
Push(1001) // code du label retour_001
Goto fact
retour_001:
;; restauration : en haut le resultat de l'appel à f(x-1)
;;                en dessous la sauvegarde de tout à l'heure
Swap
AStore(0) ;; qui remet x dans v0
Unbox ;; pour préparer la multiplication
ALoad(0) ;; pour le *x
Unbox
IMul    = IOp(IntOp.Mul)
Box
end002:
Swap ; pour mettre en haut le code du label de retour
     ; et le résultat en dessous
     ; ATTENTION: en cours j'avais mis ce swap après le label dispatch
     ; ci-dessous, mais le mettre se généralise mieux au appels indirects
Goto dispatch:
;; fin du corps de fact

; zone d'aiguillage :
;;    le haut de pile est un code de label
;;    et le Tableswitch fait sauter au label correspondant
dispatch:
Tableswitch(1000,List(retour_000, retour_001),oups)

oups:
;; on n'est pas sensé arriver ici, mettre par exemple un message d'erreur
```

## Comment faire si on a plusieurs variables à sauvegarder/restaurer ?

P.ex pour sauvergarder `v0 v1 v2` :

```
ALoad(0)
ALoad(1)
ALoad(2)
```

Et la restauration correspondante :

```
Swap
AStore(2)
Swap
AStore(1)
Swap
AStore(0)
```

En effet, au moment de la restauration, la pile contiendra `valeur0 valeur1 valeur2 resultat`
(gauche = en bas de la pile, droite = en haut). Les `swap` permettent
d'aller accéder aux valeurs sauvegardés, qui sont sous le resultat de
l'appel de fonction. 

## Pourquoi ces Unbox;Box générés par la compilation ?

Par exemple, le schéma "général" de la compilation d'une operation
arithmétique telle que `e1 * e2` :

```
Compilation(e1)
Unbox
Compilation(e2)
Unbox
IMul
Box
```

Et la compilation de `Num(n)` est maintenant `Push(n);Box` ce qui peut
donc être suivi d'un `Unbox` si une opération arithmétique est à
venir. Par la suite, vous pourrez optimiser cela:

 - soit en ajoutant une fonction de compilation dédiée à obtenir un
   résultat non-boxée (p.ex. à utiliser à la place du
   `Compilation(e1);Unbox` ci-dessus)

 - soit via une passe d'optimisation a posteriori supprimant les `Box;Unbox`

## Exemple de calcul de fact(3)

Déroulement du calcul : à retrouver vous-même. Au besoin, nettoyer et
intégrer l'exemple précédent pour `fact` dans votre code scala et
lancer l'interpréteur Javic fourni, avec `-trace` pour un affichage
détaillé de l'état de la JVM à chaque étape:

```
sbt> run -trace examples/fact.fx
```

## Appels fonctions indirectes

Il s'agit des `Call (e, ...)` où l'expression `e` indiquant la
fonction à appeler n'est pas directement un `Fun(f)`. Par exemple `e`
peut être un `Var(...)` (cf. les exemples `listmap.fx` ou `compose.fx`).
Ou bien encore un `If(...,Fun(f),Fun(g))` (ok, exemple assez
artificiel, mais pourquoi pas). Ou même `e` peut lui-même être
un `Call(...)` (en programmation fonctionnel, un appel de fonction
peut retourner une fonction).

En tout cas, `e` doit être une expression qui *après calcul* donne
un nom de fonction (cf. `RFun` dans `FopixInterp`). Mais un
compilateur ne *doit pas* faire ce calcul pour savoir quelle fonction
lancer, ce n'est pas son rôle. Le compilateur met juste tout en place
pour que le calcul se déroule lorsque l'utilisateur lancera le `.class`.

Rappel sur la syntaxe concrète Fopix : 
 - `f(..,..,..)` donne l'AST `Call(Fun(f),List(..,..,..))`
 - `?(e)(..,..,..)` donne l'AST `Call(e,List(..,..,..))`
   (c'est cette forme qui est la plus générale et peut donc donner
    des appels indirects)
 - Et au fait, `&f` est la syntaxe donnant un `Fun(f)` tout seul.
 
Bref, `f(x,y)` est juste une syntaxe courte et pratique pour la forme
générale `?(&f)(x,y)`, les deux donnant l'AST `Call(Fun(f),...)`.

Avec un appel de fonction "direct", on connait à compile-time la
fonction à déclencher, donc un simple `Goto f` convient.

Pour un appel indirect, il nous fait un saut "dynamique", dont la
destination est décidée à run-time. On utilise pour cela la même
idée du `Tableswitch` final (après le label `dispatch:`), et la
même idée d'une correspondance entre des labels et des entiers
servant de code pour ces labels. Simplement, au lieu de faire
cela juste pour les labels de retour comme auparavant, on fait
cela également pour les labels de fonctions (au moins ceux pour
lesquels un `Fun(f)` isolé est présent quelque-part dans le code).

#### Compilation du Fun(f)

Comment compiler la construction `Fun(f)` (syntaxe concrète `&f` ) :

```
Push(numero_correspondant_a_f)
Box
```

#### Compilation générale du Call(...)

S'il s'agit d'un appel direct `Call(Fun(f),...`, c'est ce qu'on a déjà
vu précédemment:

```
;; (1) sauvegarde des variables dans la pile
;; (2) calcul des arguments
;; (3) stockage des arguments
;; (4) Push code du label de retour
Goto f
retour_XYZ:
;; (5) restauration des variables
```

Sinon, pour un `Call(e,...)` avec `e` quelconque :

```
;; (1) sauvegarde des variables dans la pile
;; (2) Push code du label de retour (dans la pile)
;; (3) compilation de l'expression e
Unbox (du code correspondant à e)
;; (4) calcul des arguments
;; (5) stockage des arguments
Goto dispatch
retour_XYZ:
;; (5) restauration des variables
```

Et dans la zone d'aiguillage, les labels des fonctions
atteignables par des appels indirects seront à mettre aussi
dans le `Tableswitch`, à l'emplacement idoine selon le code
qui aura été choisi pour ce label. Par exemple:

```
dispatch:
Tableswitch(1000,[retour_000,f,retour_002,g], oups)
```

Dans ce cas, le code 1001 emmène sur `f`, le code 1003 emmène sur `g`.


## Factopt : un exemple de fonction recursive terminale

```
def fact(x,r) = if x == 0 then r else fact(x-1,r*x)
val _ = print_int(fact(10,1))
val _ = print_string("\n")
/* answer: 3628800 */
```

Déjà, lors des appels récursifs, on peut ici ne pas sauvegarder
l'état courant des variables `x` et `r`, vu que ces variables ne
sont pas réutilisées après le retour de l'appel récursif.

Pour le reste, si on compile comme vu auparavant, sans optimisation
particulière à part l'absence de sauvegarde/restauration :

- Etat de la JVM lors du premier `goto fact` (celui causé par l'appel `fact(10,1)`):
  `v0=10, v1=1, pile=code_retour_000` où `retour_000` est le label de
  retour de cet appel à fact.

- Etat de la JVM au premier sous-appel interne `fact(x-1,r*x)`:
 `v0=9, v1=10, pile=code_retour_000,code_retour_001` où `retour_001`
  est le label suivant cet appel interne à `fact` (et juste avant la
  fin du corps de `fact`.
  
- Etat de la JVM au dernier sous-appel interne `fact(x-1,r*x)`:
 `v0=0, v1=3628800 pile=code_retour_000,code_retour_001,...,code_retour_001`.

Ensuite, les fins successives des exécutions de chacun des corps de
`fact` pour chacun des sous-appels va faire une succession de
`goto dispatch`, `tableswitch` menant à `retour_001`, puis
`goto dispatch` de nouveau (ce `retour_001` étant à la fin de `fact`),
puis `tableswitch`, et ainsi de suite jusqu'à dépiler tous les codes
de retour présents dans la pile, et finalement retourner à
`retour_000` et passer à `print_int` dans le `main`.

Une ruse possible est alors de ne **pas** mettre de code de label de
retour dans le cas des appels internes à `fact` ! Après tout, il y en
a déjà un, le `code_retour_000` mis par le tout premier appel à `fact`
depuis `main`. Et donc le premier corps de `fact` qui terminera
sautera directement au tout début, finissant d'un seul coup **tous**
les sous-appels récursifs à `fact` en cours.

Bref: compilation rusée d'un appel terminal :

```
; RIEN à sauvegarder
; compiler les arguments
; les stocker
; PAS de code de label retour
Goto f
; RIEN après (car on ne reviendra pas ici! )
```

Attention, cette optimisation n'est pas à faire lors du tout premier
appel (celui depuis `main`).

## Au fait, comment sait-on si un Call est un appel terminal ?

Détection d'un appel terminal : tout se passe au niveau de l'AST.

- On regarde chaque définition de fonction `def f(x) = e`
- On parcours ensuite récursivement le corps `e` de `f` à la
  recherche des emplacements terminaux, et un appel `Call` situé
  dans à un emplacement terminal est un appel terminal.
    
  * `Let(x,e1,e2)` : Si tout le `Let` est dans une zone terminale,
    alors `e2` est encore une zone terminale (mais pas `e1`).
  * `If(e1,e2,e3)` : Si tout le `If` est dans une zone terminale,
    alors `e2` et `e3` sont des zones terminales (mais pas `e1`).
  * Toutes les autres constructions (`Op`, `Prim`, `Call`, ...)
    ne contiennent aucune sous-parties terminales : peu importe
    où se trouverait un `Call` la-dedans, il y aura des calculs
    ultérieurs à faire après le retour de ce `Call`.


 
