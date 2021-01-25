
Compilation Fopix vers Javix : quelques précisions
==================================================


## Comment compiler un If ? ##

Rappel sur les comparaisons de la JVM : `if_icmpeq label` par exemple permet de
comparer les deux entiers du haut de la pile (puis les enlever), avant de sauter
à la positition de code `label` si ces entiers sont égaux, ou au contraire
de passer à l'instruction immédiatement suivante si ces entiers sont différents.

*Question A* : que faire face à un `If(e,...,...)` lorsque `e` n'est pas directement `Op(Eq,int1,int2)` ?
Pas encore traité, à vous d'y refléchir pour l'instant. 

*Question B* : comment simuler les deux branches d'un If (le then et le else)
alors qu'on a un seul label dans un `if_icmp..` ? Partons d'un cas simple,
par exemple un `If (Op(Eq,int1,int2), exprthen, exprelse)`. Cela donne:

```
 ; compilation de int1
 ; compilation de int2
 if_icmpne labelELSE_N    ; attention, opérateur inverse !

 ;
 ; compilation de exprthen
 ; 
 goto labelEND_K

 labelELSE_N:
 ; on arrive ici si l'égalité n'était PAS vraie
 ;
 ; compilation du exprelse
 ; 
 
 labelEND_K:
 ; la suite ...
```


## Compilation du "main" ##

Ce que j'appelle "main" d'un de nos exemples, c'est toutes les déclarations de valeurs globales
(`val ...`) successives. Rappel : là il y a un ordre, chaque `val` peut utiliser un des noms
de `val` précédents (sauf si le nom est `_` auquel cas le résultat associé est perdu).
Ce n'est pas comme les définitions de fonctions qui peuvent venir dans n'importe quel ordre
(et s'appeler librement les unes les autres, récursivité mutuelle). Rappel également,
ces valeurs globales `val` ne sont *pas* utilisables depuis le corps d'une fonction.


Si le "main" d'un exemple Fopix ressemble à ceci :

```
val toto = e0
val titi = e1 /* où e1 peut réutiliser toto */
val truc = e2 /* où e2 peut réutiliser toto et titi */
val _ = e3 /* la valeur calculée pour e3 ne sera pas sauvegardée,
              ce e3 n'est donc interessant que pour les print qu'il contient */
```

Traduit en du code JVM séquentiel, à mettre au début de son programme :


```
;
; Bloc issu de la compilation de e0
; Store v0 (qui est la variable JVM associé à la variable fopix toto)
;
; Compil de e1 (peut faire des Load v0 lorsque nécessaire)
; Store v1 (associé à titi)
; 
; Compil de e2 ( ... Load v0 ... Load v1 ....) 
; Store v2 (associé à truc)

; Compil de e3
; Drop (comme le nom de variable est _)

; Return ; avec une pile vide normalement à ce moment
```


## Compilation des fonctions ##

L'idée d'une fonction c'est de pouvoir être utilisée plusieurs fois,
et donc de factoriser les instructions correspondantes. On verra
que dans certains cas simples on peut remplacer un appel de fonction
par son corps directement dans le code source (optimisation nommé
dépliage ou "inlining"), mais ce n'est *pas* à faire en général.

Organisation s'il y a des fonctions dans l'exemple à compiler :

```
;
; MAIN (cf avant, obtenu à partir des Val)
;
; return

; La zone des fonctions (issue de la compilation des Def)
; Normalement on n'y vient jamais via exécution séquentiel (cf return ci-dessus)
; mais on peut sauter (lors d'un Call à un des labels ci-dessous)

label_f:
 ; preambule de f : a priori vide
 ; compilation du corps de f
 ; epilogue de f : 
 ; "saut retour"  : saut "dynamique" vers la ligne après l'appel à f (cf ci-dessous)

label_g:
  ; compilation du corps de g
  ; "saut retour"

; ...

;; + une zone finale : l'aiguillage (dispatch, soit pour appels indirect
;                                              soit pour les retours)
; Voir ci-dessous

```

## Trajet lors d'un appel de fonction ##

Difficulté : en général, une fonction ne sait pas quel endroit du programme
l'a appelé (plusieurs "sites" d'appels, p.ex. une fonction récursive a
un premier lancement (dans le "main" p.ex.) et un appel à elle-même depuis
son propre corps. Par exemple:

```
def f(x) = x+x
val _ = f(1)+f(2)
```

donnera (avec mes essais de flèches numérotés pour décrire l'exécution...)

```
; MAIN:
;
; compilation de Call(Fun f,[Num 1])
; contiendra un
; goto label_f                             ----(1)------->
; <-------(2)------
;
; compilation de Call(Fun f,[Num 2])       ----(3)------->
; contiendra un
; goto label_f
; <------(4)------
;
; iadd
; drop
; return

label_f:           <---(1)---- puis plus tard <----(3)-----
 ; compilation du corps de f
 ; "saut retour" : goto vers ---(2)---> ou vers ---(4)---> ??
```

Avant de détailler ce "saut retour" dynamique, parlons déjà de la compilation
de l'appel de fonction lui-même


## Compilation du Call proprement dit ##

Pour l'instant, on parle ici d'un appel "direct", c'est-à-dire un
`Call(Fun "f", [arg0,arg1,arg2]))`.

**Convention : les valeurs des arguments seront dans les premières variables de JVM **.
Ainsi `arg0` sera dans `v0`, etc.

On aura donc besoin d'archiver les anciennes valeurs de `v0` ... `vk` le temps d'un appel
de fonction, et les restaurer ensuite (si ces valeurs sont encore utiles). On en reparlera,
il s'agit de `Load` de ces variables sur la pile pour archiver, et de `Store` pour restaurer.

#### 1er essai de call ####

```
;; tout d'abord, archivage si besoin
;; puis : 
; compilation de arg0
; store dans v0
; compilation de arg1
; store dans v1
; compilation de arg2
; store dans v2
goto label_f
retourIci_NN:  ; label qui sera utilisé pour revenir ici
;; restauration des variables
```

Ce premier essai de mise en place des arguments dans les `v0` ... `vk` est **faux**.
Exemple Fopix : 

```
let x = 3
in f (x+1,x+2,x+3)
```

en pseudo-JVM:

```
Push 3
Store v0  ;; à cause du let

; compilation de arg0
Load v0 ; pour retrouver x
push 1
iadd  ; fini la compil de arg0
istore v0

; compilation de arg1
Load v0 ; pour retrouver x /// OUI mais non, v0 contient le préparatif pour arg0
push 2
iadd  ; fini la compil de arg0
...
```


#### Version corrigée de la mise en place des arguments ####

````
;; tout d'abord, archivage si besoin
;; puis : 
; compilation de arg0
; compilation de arg1
; compilation de arg2
; store dans v2
; store dans v1
; store dans v0
goto label_f
retourIci_NN:  ; label qui sera utilisé pour revenir ici
;; restauration des variable
```

Il faut compiler tous les arguments puis *après* faire tous les stores. Ainsi
l'environnement de calcul de ces arguments reste bien cohérent. A l'exécution,
toutes les valeurs de ces arguments s'empilent progressivement, puis les store
les dépilent (attention à l'ordre des stores !).



## Le corps d'une fonction ##

Très simple:

```
label_f:
 ; compilation du corps de f
 goto label_dispatch
```

La fonction reçoit ses arguments dans les variables JVM `v0` ... `vk` si elle a (k+1) arguments.
A l'exécution, son calcul se déroule, mettant à la fin le résultat sur le haut de pile.
Le goto final envoie vers la zone globale chargé du saut retour dynamique (placée par exemple
toute à la fin du programme), aiguillage ou dispatch en anglais.

**Convention : quand on revient d'un appel de fonction, le résultat est sur le haut de pile**


## Le mécanisme de retour dynamique ##

On va utiliser l'instruction `tableswitch` pour pouvoir faire un saut dynamique,
une sorte de `goto` dont le label n'est pas connu statiquement dans le source
du programme. On pourrait aussi utiliser une ribambelle de `if_icmp` mais cela
serait moins léger!

**TODO** : comparaison avec `jsr` et `ret`


Instruction: `tableswitch (1000,[label0,label1,label2],labelautre)`
Le premier nombre est un "offset", les indices sont décalés de cette quantité.
Ce n'est pas indispensable d'utiliser un offset différent de 0, mais cela donne
des indices repérables facilement dans le programme finale. Je suggère d'utiliser
des indices numérotés à partir de 1000.

Ensuite, lors d'un `tableswitch(1000,[label0,label1,label2],labelautre)`, si en haut de pile on trouve :
 * 1000 : on l'enleve et on saute à `label0`
 * 1001 : on l'enleve et on saute à `label1`
 * 1002 : on l'enleve et on saute à `label2`
 * n'importe quoi d'autre : on l'enleve et on saute à `labelautre`


Maintenant, à chaque label où on voudra pouvoir sauter dynamiquement, on va associer
un nombre qui sera son indice dans un grand `tableswitch` final. Ici par exemple,
`label0` aura pour code 1000, `label1` aura pour code 1001, etc. Ces nombres encodant
des labels nous serviront "d'adresses de retour", partout où la JVM accepte un entier
mais pas un label. Par exemple dans un Push sur la pile. 

Au final :


```
;; au niveau du call, c'est comme avant, avec juste un push en plus devant le goto f

;; archivage des variables 
;; mise en place des arguments
push (numero du label de retour, ici 100N)
Goto f   ---(1)--->
retourNN: (label dont le code est 100N)    <---(3)---
;; restauration des variables

;;Corps de f: 
label_f:                   <---(1)----saut de l'aller
 ... ; comme avant
 ... ; resultat en haut de pile et juste en dessus l'"adresse de retour"
 goto label_dispatch       ---(2)--->saut en fin de f

;; Table globale tout à la fin
label_dispatch:   <---(2)----
 swap ; pour échanger "l'adresse de retour" et résultat en haut de pile
 tableswitch (1000, [ ...  retourNN (en position NN)   ...  ]        ---(3)------->
```


## Et les appels indirects de fonction ? ##


Un appel indirect est un `Call (e, args)` où `e` n'est pas syntaxiquement un `Fun`
dans l'AST du programme. Il faudra faire exécuter le code correspondant à l'expression
`e` pour savoir où aller.

On en parlera la prochaine fois.

