#lang racket

(require "../share/gf.rkt")
(require "../share/poly.rkt")

(provide (contract-out
          [init-t (-> natural? string?)]
          [euc-init (-> string? natural? pair?)]
          [euclideans (-> string? natural? string?)]
          ))

(define (init-t t)
  (format "x~a" (* 2 t)))

(define (euc-init syndrome_poly t)
  (let (
        [init_poly #f]
        [first_syndrome_n #f]
        [align1 #f]
        [poly_align1 #f]
        [poly_add1 #f]
        [poly_add1_align #f]
        [align2 #f]
        [poly_align2 #f]
        [poly_add2 #f]
        )

    (set! init_poly (init-t t))
    
    (set! first_syndrome_n (caar (string-a->poly syndrome_poly)))
    
    (printf "first_syndrome_n:~a\n" first_syndrome_n)
    
    (set! align1 (format "~ax" (poly-gf-n-multiply-align first_syndrome_n 1)))

    (printf "align1:~a\n" align1)
    
    (set! poly_align1 (poly-gf-n-multiply syndrome_poly align1))

    (printf "init_poly:~a\n" init_poly)

    (printf "poly_align1:~a\n" poly_align1)
    
    (set! poly_add1 (poly-n-add init_poly poly_align1))

    (printf "poly_add1:~a\n" poly_add1)
    
    (set! poly_add1_align (caar (string-a->poly poly_add1)))

    (printf "poly_add1_align:~a\n" poly_add1_align)
    
    (set! align2 (number->string (poly-gf-n-multiply-align first_syndrome_n poly_add1_align)))

    (printf "align2:~a\n" align2)
    
    (set! poly_align2 (poly-gf-n-multiply syndrome_poly align2))
    
    (printf "poly_align2:~a\n" poly_align2)

    (set! poly_add2 (poly-n-add poly_add1 poly_align2))

    (printf "poly_add2:~a\n" poly_add2)
    
    (cons poly_add2 (string-append align1 "+" align2))))

(define (euclideans syndrome_poly t)
  "")
