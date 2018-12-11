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
    
    (printf "~a\n" init_poly)
    
    (set! first_syndrome_n (caar (string->poly syndrome_poly)))
    
    (set! align1 (format "~ax" (poly-gf-n-multiply-align first_syndrome_n 1)))
    
    (set! poly_align1 (poly-gf-n-multiply syndrome_poly align1))
    
    (set! poly_add1 (poly-n-add poly_align1 poly_add1))
    
    (set! poly_add1_align (caar (string->poly poly_add1)))
    
    (set! align2 (number->string (poly-gf-n-multiply-align first_syndrome_n poly_add1_align)))
    
    (set! poly_align2 (poly-gf-n-multiply syndrome_poly align2))
    
    (set! poly_add2 (poly-n-add poly_add1 poly_align2))
    
    (cons poly_add2 (string-append align1 "+" align2))))

(define (euclideans syndrome_poly t)
  "")
