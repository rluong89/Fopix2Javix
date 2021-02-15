Notes de la séance 5 de TransProg M2
====================================

## Suite du travail sur Fopix2Javix : Compilation directe de Fopix vers Javix

#### Exemples complets : fact et factopt

Les [gribouillis du prof](seance5-live.md) en live pendant la séance 5, un peu reformatés.

#### Booléens seuls, If sans comparaisons

Il est suggéré de coder les booléens comme des entiers, par exemple
0 pour faux et 1 pour vrai.

Pour un `If(e1,e2,e3)` avec `e1` qui n'est pas directement un
`Op(cmp,...,...)`, la compilation de e1 doit à l'exécution mettre un
entier 0 ou 1 sur la pile, et donc ce `If` peut être traduit en une
comparaison de cet entier avec 0 ou 1 (via un `Ificmp` ou mieux un
`If` de Javix, celui qui compare un entier et 0).

Et pour un `Op(cmp,...,...)` sans `If` autour de lui, c'est l'inverse,
il faudra le compiler comme une instruction mettant 0 ou 1 sur la
pile, là encore via un `Ificmp` dont les blocs suivants "then" et
"else" seraient des `Push(1);Box` et `Push(0);Box`.

#### Appel de fonction indirect

Cf le premier lien ci-dessus.

#### Appels recursifs terminaux (tail calls)

Cf le premier lien ci-dessus.

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
