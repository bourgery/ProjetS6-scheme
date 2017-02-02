#lang racket
(provide (all-defined-out))

(require "chaine_binaire_et_ensemble_chaine.rkt")
(require "Methode_facteur_premier.rkt")
(require "Verification.rkt")

; Precond : La chaine d'addition l est correcte.
(define (cherche-facteurs-aux n x1 l)   ; retourne la valeur x2 dans l tel que n = x1 + x2
  (cond [(null? l) -1]
        [(= n (+ x1 (car l))) (car l)]
        [else (cherche-facteurs-aux n x1 (cdr l))]))
  
  
(define (cherche-facteurs n l)   ; retourne le couple (x1 x2) tel que x1, x2 appartiennent a l et x1 + x2 = n
  (let ((x2 (cherche-facteurs-aux n (car l) l)))
    (cond [(< 0 x2) (list (car l) x2)]          ;(= n (+ (car l) x2))
          [else (cherche-facteurs n (cdr l))])))


; Supprime les doublons dans la liste l, que le doublon soit un element ou une liste.
(define (remove-doublon l)
  (cond
    [(null? l) '()]
    [else (cons (car l) (remove-doublon (remove* (list (car l)) l)))]))

; Pour remplacer les occurences de a par b dans list.
(define (replace a b list)
  (cond
    [(null? list) '()]
    [(list? (car list)) (cons (replace a b (car list)) (replace a b (cdr list)))]
    [(= (car list) a) (cons b (replace a b (cdr list)))]
    [else (cons (car list) (replace a b (cdr list)))]))

; Cette fonction retourne vrai si x n'est pas dans l, faux sinon.
(define (not-in x l)
  (if (null? l)
      true
      (and (not (= x (car l))) (not-in x (cdr l)))))

(define (noms-registres ch prec utile ch-fin prec-fin) ;renvoie un couple de liste : - le "nom" de variable a gauche du egal
                 (if (null? ch)                        ;                             - le "nom" du couple des variable a droite du egal
                     (values ch-fin prec-fin)
                     (let ([tete (car ch)]
                           [a (caar prec)]
                           [b (cadar prec)])
                       (cond 
                         [(and (not-in a utile) (= a b)) (noms-registres             ;si tete = a*a et a est la derniere variable calculee
                                                          (cdr (replace a tete ch))
                                                          (cdr (replace a tete prec))
                                                          utile
                                                          (cons tete ch-fin)
                                                          (cons (list tete tete) prec-fin))]
                         [(not-in a utile) (noms-registres                         ;si tete = a * b et a ne sera plus utile pour le calcul d'autres variables et a != b
                                            (cdr (replace a tete ch))
                                            (cdr (replace a tete prec))
                                            (cons b utile)
                                            (cons tete ch-fin)
                                            (cons (list tete b) prec-fin))]
                         [(not-in b utile) (noms-registres                        ; si a est utile pour d'autres variables et b ne l'est pass
                                            (cdr (replace b tete ch))
                                            (cdr (replace b tete prec))
                                            utile
                                            (cons tete ch-fin)
                                            (cons (list a tete) prec-fin))]
                         [else (noms-registres                          ; si a et b sont utiles pour d'autres variables
                                (cdr ch)
                                (cdr prec)
                                utile
                                (cons tete ch-fin)
                                (cons (list a b) prec-fin))]))))
 


; Cette fonction retourne la chaine d'addition la plus optimisee
(define (chaine-add n)
  (let
      ((ch-binary (chaine-binaire n))
       (ch-primary (chaine-plus-petit-diviseur n)))
    (if (<= (length ch-binary) (length ch-primary))
        ch-binary
        ch-primary)))

; Cette fonction compte le nombre de registre minimum necessaire pour generer le code de la puissance n 
(define (count-nb-register n)
   (let*-values
       (((l lf) (noms-registres (reverse (cdr (chaine-add n))) (reverse (liste-facteurs (chaine-add n))) (cons n '(1)) '() '())))
     (add1 (length (remove-doublon l)))))



; Cette fonction calcule la liste des facteurs de n dans 
(define (liste-facteurs chaine)
  (map (lambda (x) (cherche-facteurs x chaine)) (cdr chaine)))
   
; Cette fonction compte le nombre de registre minimum necessaire pour generer le code correspondant a la chaine d'addition 
(define (count-nb-register-chaine c)
   (let*-values
       (((l lf) (noms-registres (reverse (cdr c)) (reverse (liste-facteurs c)) (cons (car (reverse c)) '(1)) '() '())))
     (add1 (length (remove-doublon l)))))
