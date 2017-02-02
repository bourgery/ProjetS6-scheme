#lang racket
(provide (all-defined-out))

(define (est-dedans entier l);fct determinant si un entier n se trouve dans la liste de liste l(à changer avec un map)
  (if (null? l)
      #f
      (or (member entier (car l)) (est-dedans entier (cdr l)))))

(define (ajoute-elt pere pere-var reste-de-larbre l2);fonction créant le(s) fils de pere
  (if (null? pere-var)
      l2
      (if (not (est-dedans (+ (car (take-right pere 1)) (car pere-var)) reste-de-larbre));si la chaine d'addition n'a pas été crée alors on la créer
          (ajoute-elt pere (cdr pere-var) reste-de-larbre (append l2 (list (append pere (list (+ (car (take-right pere 1)) (car pere-var)))))))
           (ajoute-elt pere (cdr pere-var) reste-de-larbre l2))));sinon on regarde les autres fils potentiels 

(define (gen-arbre-aux n l1 l2 l3);fonction auxiliaire generant l'arbre de puissance
;  le principe de cette implémentation est le suivant :
;  l1, l2, l3 sont des listes de listes : l1 contient l'arbre, l2 contient temporairement les fils
;  qui s'ajoutent au fur et à mesure à l'arbre, l3 contient temporairement les pères.
;  Quand l2 n'est pas null on ajoute les fils qu'elle contient à l'arbre l1 ainsi qu'à l'arbre l3(puisque ces fils sont de futurs pères).
;  Quand l2 est null  c'est qu'il est temps de creer une nouvelle generation qui sont les fils des peres contenus dans l3, 
;  ces fils sont stockés dans l2;
;  On réitère récursivement
  
  (if (= n (car (take-right (car (take-right l1 1)) 1)));on s'arrete dès qu'on a généré la chaine d'addition de l'entier n voulue
      l1;on renvoie l'arbre
      (if (null? l2)
          (gen-arbre-aux n l1 (ajoute-elt (car l3) (car l3) l1 l2) (cdr l3))
          (gen-arbre-aux n (append l1 (list (car l2))) (cdr l2)  (append l3 (list (car l2))))
          )))


(define (gen-arbre n);fonction final generant l'arbre de puissance
  (gen-arbre-aux n '((1)) '((1 2)) '()))






