#lang racket

(require rackunit)

(require "../../src/field-math.rkt")

(define (_galios-poly-add poly1 . rst)
  (index_coe_pairs->poly
   (let ([combine_hash (make-hash)])
     (let loop ([loop_polys `(,poly1 ,@rst)])
       (printf "loop_polys:~a\n\n" loop_polys)
       (when (not (null? loop_polys))
         (map
          (lambda (p)
            (if (hash-has-key? combine_hash (car p))
                (let ([coe_bitwised (bitwise-xor (cdr p) (hash-ref combine_hash (car p)))])
                  (if (= coe_bitwised 0)
                      (hash-remove! combine_hash (car p))
                      (hash-set! combine_hash (car p) coe_bitwised)))
                (hash-set! combine_hash (car p) (cdr p))))
          (poly->index_coe_pairs (car loop_polys)))
         (loop (cdr loop_polys))))
     (hash->list combine_hash))))

(check-equal? (_galios-poly-add "9" "9") "")
