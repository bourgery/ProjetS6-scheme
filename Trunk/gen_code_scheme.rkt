#lang racket
(provide (all-defined-out))

(require "Methode_facteur_premier.rkt")
(require "Verification.rkt")

(define-namespace-anchor anchor)
(define ns (namespace-anchor->namespace anchor))
(define (eval-expr expr) (eval expr ns))

(define (cherche-somme-liste-aux e s acc);Fonction qui renvoie x tel que e = s + x et x appartient a acc trier trie dans l'ordre decroissant
    (cond
    [(< e (+ s (car acc))) (cherche-somme-liste-aux e s (cdr acc))]
    [(> e (+ s (car acc))) -1]
    [else (car acc)]))

(define (cherche-somme-liste tete acc);Fonction qui renvoie '(x y) tel que x + y = tete avec x et y appartenant a acc trie dans l'ordre decroissant
  (let ([element (cherche-somme-liste-aux tete (car acc) acc)])
  (cond
    [(null? acc) raise "Impossible\n"]
    [(= -1 element) (cherche-somme tete (cdr acc))]
    [else (cons element (list (car acc)))])))

(define (gen-list n1 n2);Fonction qui renvoie (x(n1 + n2) (mult x(n1) x(n2)))
  (list (append (list (concat "x" (+ n1 n2)) (append '(mult)
                                               (append (list (concat "x" n1)) (list (concat "x" n2))))))))

(define (gen-let-aux l acc);Fonction permettant d'avoir le corps du let*
  (let ([queue-sans-2-premier cddr]
        [deuxieme-element-couple cadr])
   (if (null? l)
     '()
     (if (null? acc)
         (append (gen-list 1 1) (gen-let-aux (queue-sans-2-premier l) '(2 1)))
         (let ([couple (cherche-somme-liste (car l) acc)])
         (append (gen-list (car couple) (deuxieme-element-couple couple))
                 (gen-let-aux (cdr l) (cons (car l) acc))))))))
         
(define (gen-let n chaine);Fonction permettant d'avoir le corps de la fonction
  (let ([l chaine])
  (append '(let*) (append (list (gen-let-aux l'())) (list (concat "x" n))))))

(define (gen-code-scheme n chaine);Fonction permettant d'avoir la fonction calculant exp n d'un nombre
  (append (append '(lambda) (list '(mult x)) (list (gen-let n chaine)))))

(define (concat s n);Fonction permettant d'avoir un symbole sn
  (if (= n 1)
      (string->symbol s)
      (string->symbol (string-append s (number->string n)))))

(define (evaluation x chaine);Fonction evaluant automatiquement le code genere en scheme
  (let* ([dernier (lambda (l) (car (take-right l 1)))]
         [liste (gen-code-scheme (dernier chaine) chaine)])
    ((eval-expr liste) * x)))
    
