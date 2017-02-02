#lang racket
(require "../Trunk/gen_code_scheme.rkt")
(provide (all-defined-out))

(define (test-gen-code-scheme)
  (begin (display "Debut du test pour savoir si le code genere est bon : ")
         (if (equal? (gen-code-scheme 7 '(1 2 3 6 7)) '(lambda (mult x) (let* ((x2 (mult x x)) (x3 (mult x x2)) (x6 (mult x3 x3)) (x7 (mult x x6))) x7)))
             (display ".")
             (raise "Erreur"))
         (if (equal? (gen-code-scheme 7 '(1 2 3 6 7)) '(lambda (mult x) (let* ((x2 (mult x x)) (x3 (mult x x2)) (x3 (mult x3 x3)) (x7 (mult x x6))) x7)))
             (raise "Erreur")
             (display "."))
         (if (equal? (gen-code-scheme 2 '(1 2)) '(lambda (mult x) (let* ((x2 (mult x x))) x2)))
             (display ".")
             (raise "Erreur"))
         (display "OK\n")))

(define (test-evaluation)
  (begin (display "Debut du test pour savoir si l'evaluation effectuee est correcte : ")
         (if (= (evaluation 2 '(1 2 3)) 8)
             (display ".")
             (raise "Erreur"))
         (if (= (evaluation 2 '(1 2 4)) 8)
             (raise "Erreur")
             (display "."))
         (if (= (evaluation 0 '(1 2 3 6 7 14 28)) 0)
             (display ".")
             (raise "Erreur"))
         (display "OK\n")))

(test-gen-code-scheme)
(test-evaluation)