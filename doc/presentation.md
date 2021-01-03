% Transformation de programmes
% Pierre Letouzey
% 9 décembre 2020

## Transformation de programmes, késako ?

 - Ancien nom : compilation avancée
 - Accent sur les étapes *intermédiaires* d'un compilateur
 - Ni parsing ni assembleur x86 (ou en option seulement), mais:
 - Remaniement d'AST (arbres de syntaxe)
 - Passages progressifs de langages haut-niveau à bas-niveau
 - Comment rendre les récursivités terminales ?
 - Qu'est-ce qu'une mise en CPS (continuation passing style) ?

## Prérequis

 Plutôt des recommandations : 

 - Avoir suivi M1 Compilation est un plus
   * thème commun, même si on verra des techniques différentes

 - Connaître au moins un langage fonctionnel
   * On programmera dans le fragment fonctionnel de Scala

## Projet

 Transformation d'un langage idéalisé mais représentatif
 (traits impératifs + fonctionnels) vers du bytecode JVM
 en assurant une récursivité efficace.

## Evaluation

  65% projet + 35% examen


