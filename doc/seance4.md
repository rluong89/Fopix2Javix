Notes de la séance 4 de TransProg M2
====================================

## Suite du travail sur Fopix2Javix : Compilation directe de Fopix vers Javix

Première tâche pour le projet : écrire une traduction directe de Fopix vers Javix.
A continuer dès aujourd'hui avec moi (TP4).

#### Validateur JVM

Il est *normal* que le code Javix produit par Fopix2Javix soit
fréquemment refusé par le validateur de la JVM, vu que notre usage
de la pile ne sera pas forcément prévisible à l'avance.
Du coup pour l'instant: `java -noverify FooBar` 
  
#### Arrêt

l'exécution de votre code Javix doit terminer par un `Return`.
Il n'y a donc pas de réponse finale, par contre vos exemples
contiendront sans doute des affichages (`IPrint` et `SPrint`).
 
#### Convention d'appel de fonction:
 
  - Si la fonction a n arguments, lors de l'appel ces n arguments
    seront dans les variables 0,1,...,n-1 de la JVM
  
  - Avant de mettre en place les arguments dans ces variables,
    il convient de sauvegarder le contenu antérieur de ces variables
    (au moins lorsque ce contenu resservira). Cette sauvegarde se
    fait sur la pile. Du coup, une restauration correspondante est
    à faire juste après le retour de l'appel de fonction.

  - Juste avant le saut vers le code de la fonction, l'adresse de
    retour doit être mis sur la pile. Par adresse de retour, on entend
    ici un entier choisi pour correspondre au label de retour (celui
    placé devant l'instruction suivant le saut). La correspondance
    entre entier et label se fera dans un `Tableswitch` servant
    d'aiguillage (zone de "dispatch" à la fin du code compilé).
  
  - Lors du retour de l'appel de fonction, la valeur resultat devra
    être au sommet de la pile.

#### Boxing

La pile peut contenir:
 
  - des valeurs entières, dites aussi "scalaires" ou valeurs
    "non-boxés" (cf. `Push` par exemple)
   
  - des pointeurs (ou valeurs "boxées"), par exemple vers des
    tableaux, des chaînes de caractères, des objets, etc.

Pour satisfaire le "typage" de la JVM, nous utiliserons souvent
des nombres "boxés", c'est-à-dire mis dans une zone allouée dans le
tas, et accessible uniquement via un pointeur. Cela correspond
à la différence en Java entre `int` (le type primitif) et `Integer`
(l'objet contenant un entier). Ici nous utiliserons des tableaux
de taille 1 pour "boxer" nos entier, les pseudo-instructions
`Box` et `Unbox` permettant ce "boxage" et l'opération inverse.

Les opérations arithmétiques de la JVM et les comparaisons
fonctionnent seulement sur des entiers non-boxés.

Normalement les variables de la JVM peuvent également contenir
des valeurs boxées ou non-boxées (`AStore` et `ALoad` versus `IStore`
et `ILoad`). Mais le validateur de la JVM peut protester si une
variable particulière change de type de contenu. Il est donc fortement
recommandé de ne mettre que des valeur boxées dans les variables.

Pour les tableaux, seules les opérations "boxées" sont fournies:
`ANewarray`, `AAStore`, `AALoad`...

Evidemment, ces opérations d'allocation et d'accès mémoire ne sont
pas gratuites, on pourra chercher à minimiser le nombre de `Box`
et `Unbox` utilisées, par exemple via des optimisations comme la
suppression des groupes d'instuctions `Box;Unbox` ou `Unbox;Box`.

#### Appels recursifs terminaux (tail calls)

Si le code de la fonction `f` se termine par un appel à une autre
fonction `g` (qui peut être `f` de nouveau en cas de récursion),
pas besoin de sauvegarder des variables, ni de placer une adresse
de retour sur la pile : on peut s'arranger pour recycler l'adresse
de retour de l'appel en cours à `f` lors du lancement de `g` !
C'est l'optimisation des appels terminaux. Attention, le tout premier
appel de fonction ne peut être optimisé (pas encore d'adresse de
retour). Pour Fopix2Javix, cette optimisation n'est pas obligatoire
(mais recommandé). De plus, ne pas chercher pour l'instant à changer
le code que l'on compile pour le rendre récursif terminal.

#### Nombres de variables, taille de pile

Pour les premiers essais, vous pouvez indiquer à Javix un nombre de
variables et une taille de pile farfelus (p.ex. 100 et 10000).

Vous pouvez dès maintenant determiner le nombre de variables utilisées
dans le code que vous venez de compile, et indiquer ce nombre exact
dans l'entête de Javix. C'est un simple "max" sur les `ALoad` et
`AStore` que vous avez engendrés.

Par contre la taille de pile nécessaire à l'exécution d'un code Javix
peut actuellement être non-borné. Vous pouvez éventuellement mener ce
calcul dans le cas d'un code non-recursif, ou dans le cas d'un code
100% recursif terminal (avec l'optimisation correspondante).
