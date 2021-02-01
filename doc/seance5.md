Notes de la séance 5 de TransProg M2
====================================

## Suite du travail sur Fopix2Javix : Compilation directe de Fopix vers Javix

#### Exemples complets : fact et factopt

#### Appel de fonction indirect

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
