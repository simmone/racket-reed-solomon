#lang racket

(require "../share/gf.rkt")
(require "../share/poly.rkt")

(provide (contract-out
          [generator-poly (-> natural? natural? string?)]
          ))

(define (generator-poly 2^m_1 count)
  (let loop ([loop_count 2]
             [loop_poly "a0x1+a0x0"])
    (if (<= loop_count count)
        (loop
         (add1 loop_count)
         (poly-gf-n->a
          (poly-n-combine
           (poly-gf-a->n
            (poly-gf-a-multiply 2^m_1 loop_poly (format "a0x1+a~ax0" (sub1 loop_count)))))))
        loop_poly)))
