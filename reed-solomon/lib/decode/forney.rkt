#lang racket

(require "../share/lib.rkt")
(require "../share/gf.rkt")
(require "../share/poly.rkt")
(require "../share/euclidean.rkt")

(provide (contract-out
          [derivative-lam (-> string? string?)]
          [forney (->* (string? string? (listof exact-integer?)) (boolean?) (listof pair?))]
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

(define (forney lam_poly ome_poly err_places [need-express? #f])
  (let ([derivative_lam (derivative-lam lam_poly)])
    (map
     (lambda (error_index)
       (let ([ome_a #f]
             [delam_a #f]
             [cal_a #f]
             [positive_a #f]
             [modulo_a #f]
             [result_n #f])
         
         (set! ome_a (poly-gf-n-sub-x->a ome_poly (- (*2^m_1*) error_index)))
         
         (set! delam_a (poly-gf-n-sub-x->a derivative_lam (- (*2^m_1*) error_index)))

         (set! cal_a (+ error_index (- ome_a delam_a)))

         (set! positive_a (+ cal_a (*2^m_1*)))

         (set! modulo_a (modulo positive_a (*2^m_1*)))

         (set! result_n (hash-ref (*gf_aton_map*) modulo_a))

         (cons error_index result_n)))
     err_places)))
