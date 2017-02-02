#lang racket
(require math/number-theory)
(provide (all-defined-out))

(define (premier-diviseur-premier n acc);renvoie le plus petit facteur premier avec n
  (if (= 0 (modulo n acc))
      acc
      (premier-diviseur-premier n (next-prime acc))))

(define (couple-pq n);renvoie le couple p,q avec n=pq et p le + petit facteur premier de n
  (let ([p (premier-diviseur-premier n 2)])
  (list p (/ n p))))

(define (chaine-plus-petit-diviseur-aux n); renvoie la chaîne d'addition de n en suivant le principe suivant : 
  ; si n=pq avec p le plus petit facteur premier de n alors on calcule la chaine d'addition de p, suivie de la chaine de q multipliee par p
  ; si n est premier on utilise la chaine de n-1 pour construire celle de n (on met n a la suite de chaine de n-1)
  (if (= n 2)
      '(1 2)
      (if (prime? n) (cons n (chaine-plus-petit-diviseur-aux (sub1 n)))
          (let* ([couple (couple-pq n)]
                 [deuxieme-element-couple (cadr couple)])
            (append (chaine-plus-petit-diviseur-aux (car couple))
                    (map (lambda (x) (* x (car couple)))
                         (chaine-plus-petit-diviseur-aux deuxieme-element-couple)))))))

(define (unique l);fonction supprimant les doublons
  (let ([deuxieme-element-liste cadr])
  (if (null? l)
      '()
      (if (null? (cdr l))
          (list (car l))
      (if (= (car l) (deuxieme-element-liste l)) (unique (cdr l))
          (cons (car l) (unique (cdr l))))))))
      
(define (chaine-plus-petit-diviseur n);fonction final de la methode par facteur premier
  (unique (sort (chaine-plus-petit-diviseur-aux n) <)))




