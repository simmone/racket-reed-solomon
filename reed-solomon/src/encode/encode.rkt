#lang racket

(require "../lib/gf.rkt")
(require "../lib/poly.rkt")
(require "../lib/euclidean.rkt")

(provide (contract-out
          [rs-encode (->* 
                      ((listof exact-integer?) natural?) 
                      (#:bit_width natural? #:primitive_poly_value natural?) 
                      (listof exact-integer?))]
          [generator-poly (-> natural? string?)]
          ))

(define (generator-poly count)
  (let loop ([loop_count 2]
             [loop_poly "a0x1+a0x0"])
    (if (<= loop_count count)
        (loop
         (add1 loop_count)
         (poly-gf-n->a
          (poly-n-combine
           (poly-gf-a->n
            (poly-gf-a-multiply loop_poly (format "a0x1+a~ax0" (sub1 loop_count)))))))
        loop_poly)))

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
    [*gf_ntoa_map* (make-hash (hash-map (*gf_aton_map*) (lambda (a n) (cons (sub1 n) a))))])

   (let* ([generator_poly (generator-poly parity_length)]
          [message_poly (coeffients->poly-n raw_list)])

     (let-values ([(quotient remainder) 
                   (euc-divide 
                    (poly-gf-n-multiply message_poly (format "x~a" parity_length))
                    (poly-gf-a->n generator_poly))
                   ])

       (let ([result (poly-n->coeffients remainder)])
         (when (< (length result) parity_length)
           (set! result (append (make-list (- parity_length (length result)) 0) result)))
         
         result)))))
