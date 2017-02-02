#lang racket
(provide (all-defined-out))

(require "chaine_binaire_et_ensemble_chaine.rkt")
(require "Methode_facteur_premier.rkt")
(require "Verification.rkt")
(require "comptage_registre.rkt")


; Genere le code c sous forme d'une chaine de caractere pour calculer x puissance n 
(define (cons-exp-non-opt n)
  (letrec
       ((calcul (lambda (l lf c)   ;l la chaine d'addition, lf est la chaine d'addition que l'on ne modifie pas avec les appels recursifs,
                  (cond             ;c est la chaine de caracteres a retourner
                    [(null? l) c]
                    [else (calcul (cdr l) lf
                                  (string-append c "\t int x" (number->string (car l)) "=mul(x" (number->string (car (cherche-facteurs (car l) lf))) ",x" (number->string (cadr (cherche-facteurs (car l) lf))) ");\n" ))]) )))
    (string-append "int exp" (number->string n) " (int (*mul) (int, int), int x1) {\n" (calcul (cdr (chaine-add n))  (reverse (chaine-add n)) "") "\t return x" (number->string n) ";\n}")))
                                                                                                                        ; avec ou sans reverse ? (pour rapidite)

; retourne la chaine de caractere associee aux variables a initialiser pour le code c optimise. l est la liste des variables. 
(define (initialise-var l)
  (letrec
      ((init (lambda (l c)
               (cond
                 [(null? l) c]
                 [else (init (cdr l) (string-append c "\t int x" (number->string (car l)) ";\n"))]))))
    (init l "")))

(define (cons-exp n)
  (letrec
       ((calcul (lambda (l lfact c)   ;l la liste des noms des registres "a gauche" de l'affectation, lfact est la liste des noms des registres "a droite" de l'affectation
                  (cond             ;c est la chaine de caracteres a retourner
                    [(null? l) c]
                    [else (calcul (cdr l) (cdr lfact)
                                  (string-append c "\t x" (number->string (car l)) "=mul(x" (number->string (caar lfact)) ",x" (number->string (cadar lfact)) ");\n" ))]) )))
    (let*-values (((l lf) (noms-registres (reverse (cdr (chaine-add n))) (reverse (liste-facteurs (chaine-add n))) (cons n '(1)) '() '())))
      (string-append "int exp" (number->string n) " (int (*mul) (int, int), int x1) {\n" (initialise-var (remove-doublon l)) (calcul l  lf "") "\t return x" (number->string n) ";\n}"))))


; Cree un fichier "exp_n.c" qui contient le code c necessaire pour calculer x puissance n
(define (fichier-exp-c n exp)
  (begin
    (if (file-exists? (string-append "exp_" (number->string n) ".c"))
        (delete-file  (string-append "exp_" (number->string n) ".c"))
        void)
    (define out (open-output-file (string-append "exp_" (number->string n) ".c")))
    (display (exp n) out)
    (newline out)
    (close-output-port out)))

; Cree un fichier "exp_n.c" qui contient le code c necessaire pour calculer x puissance n avec optimisation de registres
(define (fichier-opt n)
  (fichier-exp-c n cons-exp))

; Cree un fichier "exp_n.c" qui contient le code c necessaire pour calculer x puissance n sans optimisation de registres
(define (fichier-non-opt n)
  (fichier-exp-c n cons-exp-non-opt))


