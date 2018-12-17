#lang racket

(require "../share/lib.rkt")
(require "../share/gf.rkt")
(require "../share/poly.rkt")
(require "../share/euclidean.rkt")

(provide (contract-out
          [derivative-lam (-> string? string?)]
          [forney (-> string? string? exact-integer? exact-integer?)]
          ))

(define (derivative-lam lam_poly)
  (let-values ([(quotient remainder)
                (euc-divide
                 (poly-n->string
                  (filter
                   (lambda (item)
                     (odd? (cdr item)))
                   (string-n->poly lam_poly)))
                 "x")])
    quotient))

(define (forney lam_poly ome_poly error_index)
  (let (
        [lam_derivative_poly #f]
        [Yj_poly #f]
        [subx_a_val #f]
        [result #f]
        )
    
    (set! lam_derivative_poly (derivative-lam lam_poly))
    
    (let-values ([(quotient remainder) (euc-divide ome_poly lam_derivative_poly)])
      (set! Yj_poly quotient))
    
    (set! subx_a_val (poly-gf-n-sub-x->a Yj_poly (- (*2^m_1*) error_index)))
    
    (set! result (hash-ref (*gf_aton_map*) (modulo (+ error_index subx_a_val) (*2^m_1*))))
    
    result))
    
    
