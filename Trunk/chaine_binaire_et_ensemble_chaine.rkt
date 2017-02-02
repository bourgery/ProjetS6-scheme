#lang racket
(provide (all-defined-out))

; crée la chaine d'addition pour expo binaire
(define (chaine-binaire n)
  (letrec
      ((cherche-deuxieme-terme (lambda (n tete l) ; "cherche-deuxieme-terme" donne le plus grand élément "e" de la liste l tel que "e + tete <= n" /\ l  doit être triée par ordre décroissant.
                   (if (>= n (+ tete (car l)))
                       (car l)
                       (cherche-deuxieme-terme n tete (cdr l)))))
       (construit-liste (lambda (n l) ; crée la chaine d'addition à l'envers
              (cond [(= (car l) n) l]
                    [(<= (* 2 (car l)) n) (construit-liste n (cons (* 2 (car l)) l))]
                    [else (construit-liste n (cons (+ (car l) (cherche-deuxieme-terme n (car l) (cdr l))) l))]))))
    (reverse (construit-liste n '(1)))))
          
;renvoie toutes les chaines d'additions possibles pour un n donné
(define (ens-chaine-add n)
  (letrec
      ((applatir (lambda (l)  ; applatie une liste de liste (de liste ...) en une unique liste
                  (cond
                    [(null? l) l]
                    [(number? (car l)) (list l)]
                    [else (append (applatir (car l)) (applatir (cdr l)))])))
       (suiv (lambda (n tete l)      ; renvoie la liste des resultats de la somme de tete par les elements de l qui sont inferieurs a n
               (cond
                 [(null? l) '()]
                 [(< n (+ tete (car l))) (suiv n tete (cdr l))]
                 [else (cons (+ tete (car l)) (suiv n tete (cdr l)))])))
       (gen-arbre (lambda (n l)    ; genere toutes les chaines d'additions pour n (les chaines sont dans des listes de listes (de liste ....)
                    (cond
                      [(= n (car l)) (reverse l)]
                      [else (values (map (lambda(x)(gen-arbre n (cons x l))) (suiv n (car l) l)))]))))
        (applatir (gen-arbre n '(1)))))