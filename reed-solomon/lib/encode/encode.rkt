#lang racket

(require "../share/lib.rkt")
(require "../share/gf.rkt")
(require "../share/poly.rkt")
(require "../share/euclidean.rkt")

(provide (contract-out
          [generate-poly (-> hash? hash? natural? natural? string?)]
          [rs-encode (->* 
                      ((listof exact-integer?) natural?) 
                      (#:bit_width natural? #:primitive_poly_value natural?) 
                      (listof exact-integer?))]
          ))

(define (generate-poly gf_aton_map gf_ntoa_map 2^m_1 count)
  (let loop ([loop_count 2]
             [loop_poly "a0x1+a0x0"])
    (if (<= loop_count count)
        (loop
         (add1 loop_count)
         (poly-gf-n->a
          gf_ntoa_map
          (poly-n-combine
           (poly-gf-a->n
            gf_aton_map
            (poly-gf-a-multiply 2^m_1 loop_poly (format "a0x1+a~ax0" (sub1 loop_count)))))))
        loop_poly)))

(define (rs-encode 
         raw_list
         parity_length
         #:bit_width [bit_width 8]
         #:primitive_poly_value [primitive_poly_value 285]
         )

  (let (
        [2^m_1 (sub1 (expt 2 bit_width))]
        [gf_aton_map #f]
        [gf_ntoa_map #f]
        [generator_poly #f]
        [message_poly #f]
        )

    (set! gf_aton_map (get-gf-aton-hash 2^m_1 primitive_poly_value))

    (set! gf_ntoa_map (make-hash (hash-map gf_aton_map (lambda (a n) (cons n a)))))
    
    (set! generator_poly (generate-poly gf_aton_map gf_ntoa_map 2^m_1 parity_length))

    (set! message_poly (coeffients->poly-n raw_list))

    (let-values ([(quotient remainder) 
                  (euc-divide 
                   (poly-gf-n-multiply message_poly (format "x~a" parity_length))
                   (poly-gf-a->n generator_poly))
                  ])

      (let ([result (poly-n->coeffients remainder)])
        (when (< (length result) parity_length)
          (set! result (append (make-list (- parity_length (length result)) 0) result)))
        
        result))))
