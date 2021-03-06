Programmation Fonctionnelle et Symbolique
==========================


Projet
========================



A remettre le vendredi 12 D´ecembre 2014 `


Jeu d’Aw´el´e
=========================


L’Aw´el´e est un jeu de strat´egie opposant deux joueurs (Nord et Sud) et se jouant sur un
plateau contenant 12 alv´eoles, 6 pour chacun des joueurs. Ce jeu qui remonte `a l’antiquit´e
est tr`es r´epandu dans le monde ; il existe un grand nombre de variantes des r`egles. Nous
adopterons les r`egles d´ecrites sur le site http://s.helan.free.fr/awale/.



1 Aw´el´e interactif
===============================
Dans cette premi`ere partie, il est demand´e de r´ealiser une interface en mode texte pour le
jeu d’Aw´el´e. On demandera aux joueurs, chacun leur tour, la position o`u ils d´esirent jouer
(entr´ee sous forme d’un num´ero de case). S’il s’agit d’une position valide, on affichera le
nouveau plateau de jeu et on passera au joueur suivant. Une position invalide devra ˆetre
signal´ee au joueur de mani`ere appropri´ee. Les cases seront num´erot´ees de 0 `a 11, les cases
5 − −6 ´etant au joueur Nord et les cases 6 − −11 au joueur Sud. Un exemple de sortie
possible est pr´esent´e Figure 1.


Modalit´es
================================================
Le point d’entr´ee du programme sera une fonction awele (), ne prenant aucun param`etre
et se terminant avec la partie.
Vous rendrez pour cette partie
– un fichier .lisp,
– ou un ensemble de fichiers .lisp dont un sert `a charger tous les autres,
– ou un fichier.asd (Makefile) et un ensemble de fichiers .lisp.


 Strat´egies non interactives
 ================================
 
 
On souhaite un programme pour faire jouer l’ordinateur, soit contre un joueur humain,
soit contre un autre programme.


2.1 Strat´egie al´eatoire
===========================================
Pour mettre en place la possibilit´e d’avoir des strat´egies non interactives, commencer par
impl´ementer une strat´egie al´eatoire : le programme joue `a une position al´eatoire parmi les
positions valides.


2.2 Intelligence artificielle
===========================================================
On souhaite dans un second temps de r´ealiser une intelligence artificielle pour le jeu
c’est-`a-dire des strat´egies plus intelligentes que la strat´egie al´eatoire. On pourra d´evelopper
une ou plusieurs strat´egies et tester leurs performances.


2.3 Tournoi
============================================
Une petite partie de l’´evaluation du projet se fera en faisant jouer les strat´egies les unes
contre les autres `a l’aide d’un programme ´ecrit par les enseignants.
Ce programme tournoi v´erifiera que les coups propos´es par une strat´egie sont des coups
valables, et les transmettra `a la strat´egie adverse. Les IA devront r´epondre en moins d’une
seconde 1
.
Une IA ne respectant pas les r`egles du jeu, ou ne r´epondant pas dans le temps imparti sera
automatiquement disqualifi´ee.




Modalit´es
====================================================
Pour cette partie, on demande un seul fichier .lisp (qui pourra ˆetre obtenu par concat´enation
d’un ensemble de fichiers .lisp convenablement comment´es).
Le programme devra fournir deux points d’entr´ee :
1. une fonction nomm´ee init-standalone (firstp) prenant en param`etre un unique
bool´een firstp indiquant qu’on joue en premier ou pas. Dans le corps de cette
fonction, on pourra initialiser les structures de donn´ees n´ecessaires au fonctionnement
de la strategie propos´ee.
2. une fonction nomm´ee main-standalone (case) prenant en param`etre le num´ero de
case jou´e par l’adversaire et retournant le num´ero de case jou´e par la strat´egie (ou
NIL si jouer est impossible).
Le param`etre case pourra prendre la valeur NIL dans les deux cas suivants :
– au premier tour si on joue en premier,
1On consid´erera les machines utilis´ees en TD comme r´ef´erence de performance.
2  au cours de la partie, si l’adversaire n’a pas pu jouer.
3 Modalit´es g´en´erales
1. Le travail doit ˆetre r´ealis´e en trinˆomes constitu´e au sein d’un mˆeme groupe de TD.
La composition des trinˆomes doit ˆetre transmise aux enseignants de TD au plus tard
le lundi 10 novembre 2014.
2. L’ensemble des fichiers associ´es au projet doit ˆetre g´er´e avec un outil de gestion de
r´evision (svn, git, ...).
3. Chaque enseignant de TD proposera sa solution pour la remise du projet (archive,
mail, soutenance,...).
4. La plus grande partie de l’´evaluation portera sur la qualit´e et l’organisation du code.
4 Interface Web (optionnel)
R´ealiser une interface Web en s’aidant de l’´ebauche d’interface Web fournie bas´ee sur la
biblioth`eque hunchentoot.
