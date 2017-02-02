#lang racket
(provide (all-defined-out))

;;Verification si une liste est une chaine

(define (est-sommable e l);Renvoie vrai s'il existe u,v appartenant a l tel que e = u + v
  (if (null? l)
      (est-sommable-aux e l 0)
      (or (est-sommable-aux e l (car l)) (est-sommable e (cdr l)))))

(define (est-sommable-aux e l s);Renvoie vrai s'il existe u appartenant a l tel que e = s + u
  (cond
    [(null? l) (= e 1)]
    [(< e (+ s (car l))) (est-sommable-aux e (cdr l) s)]
    [(> e (+ s (car l))) #f]
    [else #t]))

(define (verif-est-chaine l)
  (letrec ([verif-est-chaine-aux (lambda (l acc)
                                   (if (null? l)
                                       #t
                                       (and (est-sommable (car l)  acc)
                                            (verif-est-chaine-aux (cdr l) (cons (car l) acc)))))])
    (verif-est-chaine-aux l '())))

;;Calcul de l'exponentiation rapide

(define (cherche-somme-aux e s acc);Fonction qui renvoie x tel que e = s + x et x appartient a acc (avec acc une liste de couple) trie dans l'ordre decroissant
  (let ([tete-du-couple (caar acc)])
    (cond
    [(< e (+ s tete-du-couple)) (cherche-somme-aux e s (cdr acc))]
    [(> e (+ s tete-du-couple)) -1]
    [else tete-du-couple])))

(define (cherche-somme tete acc);Fonction qui renvoie '(x y) tel que x + y = tete avec x et y appartenant a acc (avec acc une liste de couple) trie dans l'ordre decroissant
  (let* ([tete-du-couple (caar acc)]
         [element (cherche-somme-aux tete tete-du-couple acc)])
  (cond
    [(null? acc) 1]
    [(= -1 element) (cherche-somme tete (cdr acc))]
    [else (cons element (list tete-du-couple))])))

(define (cherche-x tete acc);Fonction qui renvoie '(a b) tel x + y = tete et '((x, a) (y, b)) dans acc
  (let ([couple (cherche-somme tete acc)]
        [deuxieme-element-couple cadr])
    (cons (deuxieme-element-couple (assoc (car couple) acc)) (cdr (assoc (deuxieme-element-couple couple) acc)))))
    
(define (multiplie x tete acc);Fonction qui renvoie '(tete, x^tete)
  (let ([liste (cherche-x tete acc)])
  (list tete (* (car liste) (cadr liste)))))

(define (exp-avec-chaine-donnee x l);En n^2 en temps, l'acc est de la forme '((i, x^i) (j, x^j) ...) avec i > j 
  (letrec ([exp-avec-chaine-donnee-aux (lambda (x l acc)
    (if (null? l)
        (cadar acc) ;Pour avoir le deuxieme element du premier couple de la liste
      (if (null? acc)
          (exp-avec-chaine-donnee-aux x (cdr l) (cons (list 1 x) acc))
      (exp-avec-chaine-donnee-aux x (cdr l) (cons (multiplie x (car l) acc) acc)))))])
  (exp-avec-chaine-donnee-aux x l '())))

