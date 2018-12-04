#lang racket

(require "poly.rkt")

(provide (contract-out
          [generator-poly (-> natural? hash? hash? string?)]
          ))

(define (generator-poly count aton_map ntoa_map)
  (let loop ([loop_count 2]
             [loop_poly "a0x1+a0x0"])
    (if (<= loop_count count)
        (loop
         (add1 loop_count)
         (poly-n->a
          (poly-combine-a
           (poly-a->n
            (poly-multiply loop_poly (format "a0x1+a~ax0" (sub1 loop_count)))
            aton_map))
          ntoa_map))
        loop_poly)))
