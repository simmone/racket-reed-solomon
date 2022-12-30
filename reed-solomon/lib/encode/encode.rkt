#lang racket

(require "../share/lib.rkt")
(require "../share/gf.rkt")
(require "../share/poly.rkt")
(require "../share/euclidean.rkt")

(require "generator-poly.rkt")

(provide (contract-out
          [rs-encode (->* 
                      ((listof exact-integer?) natural?) 
                      (#:bit_width natural? #:primitive_poly_value natural?) 
                      (listof exact-integer?))]
          ))

(define (rs-encode 
         raw_list
         parity_length
         #:bit_width [bit_width 8]
         #:primitive_poly_value [primitive_poly_value 285]
         )

  (parameterize*
   ([*bit_width* bit_width]
    [*2^m_1* (sub1 (expt 2 (*bit_width*)))]
    [*primitive_poly_value* primitive_poly_value]
    [*gf_aton_map* (get-gf-aton-hash)]
    [*gf_ntoa_map* (make-hash (hash-map (*gf_aton_map*) (lambda (a n) (cons n a))))])
   
   (let* ([generator_poly (generator-poly parity_length)]
          [message_poly (coeffients->poly-n raw_list)]
          [message_length (length raw_list)])
     
     (let-values ([(quotient remainder) 
                   (euc-divide 
                    (poly-gf-n-multiply message_poly (format "x~a" parity_length))
                    (poly-gf-a->n generator_poly))
                   ])

       (let ([result (poly-n->coeffients remainder)])
         (when (< (length result) parity_length)
           (set! result (append (make-list (- parity_length (length result)) 0) result)))
         
         result)))))
