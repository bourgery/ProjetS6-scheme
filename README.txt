Ce projet a été réalisé à l’enseirb, le but de ce projet été de trouver des chaînes d’additions. Il a été réalisé dans le langage Scheme.

Nous vous proposons de tester vous-même les fonctions demandées à partir du fichier /Trunk/main.rkt. Elles y sont toutes exécutables en tapant les commandes suivantes dans la console : 

 - verif-est-chaine (liste d'entier l) : fonction qui vérifie si une liste d'exposant donnée est bien une chaîne d'addition.
 - exp-avec-chaine-donnee (entier naturel x, chaine d'addition l) : fonction permettant de calculer x à la puissance n via une chaîne d'addition de n donnée. n étant le dernier élément de la chaine l.
 - count-nb-register-chaine (chaine d'addition c) : fonction qui renvoie le nombre de registres nécessaires au calcul de x a la puissance n via la chaîne c. 
 - ens-chaine-add (entier naturel n) : fonction qui renvoie toutes les chaines d'additions possibles pour n.
 - chaine-binaire (entier naturel  n) : fonction qui renvoie la chaîne d'addition de n donnée par la méthode binaire.
 - chaine-plus-petit-diviseur (entier naturel n) : fonction qui renvoie la chaîne d'addition de n par la méthode des facteurs premiers.
 - gen-arbre (entier naturel n) : fonction qui renvoie la chaîne d'addition de n par la méthode de l'arbre de puissance.
 - gen-code-scheme (entier naturel n, chaine d'addition l) : fonction qui génère le code de l'exponentiation en Scheme.
 - evaluation (entier naturel x, chaine d'addition l) : fonction qui évalue de manière automatique le code Scheme généré pour le calcul de x à la puissance n via la chaîne l.
 - fichier-non-opt (entier naturel n) : fonction qui crée un fichier exp_n.c qui contient le code C pour calculer x puissance n sans optimisation du nombre de registres. Aucun fichier de nom "exp_n.c" ne doit être déjà présent dans le dossier courant.
 - fichier-opt (entier naturel n) : fonction qui crée un fichier exp_n.c qui contient le code C pour calculer x puissance n avec optimisation du nombre de registres. Aucun fichier de nom "exp_n.c" ne doit être déjà présent dans le dossier courant.
 

Des tests unitaires sont disponibles dans /Test/test.rkt qu'il suffit d'exécuter.
