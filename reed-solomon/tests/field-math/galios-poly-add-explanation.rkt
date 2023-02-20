#lang racket

(require rackunit)

(require "../../src/field-math.rkt")

(define (_galios-poly-add poly1 . rst)
  (items->poly
   (let ([combine_hash (make-hash)])
     (let loop ([loop_polys `(,poly1 ,@rst)])
       (printf "loop_polys:~a\n\n" loop_polys)
       (when (not (null? loop_polys))
         (map
          (lambda (p)
            (if (hash-has-key? combine_hash (PITEM-x_index p))
                (let ([coe_bitwised (bitwise-xor (PITEM-coe p) (hash-ref combine_hash (PITEM-x_index p)))])
                  (if (= coe_bitwised 0)
                      (hash-remove! combine_hash (PITEM-x_index p))
                      (hash-set! combine_hash (PITEM-x_index p) coe_bitwised)))
                (hash-set! combine_hash (PITEM-x_index p) (PITEM-coe p))))
          (poly->items (car loop_polys)))
         (loop (cdr loop_polys))))
     (map
      (lambda (p)
        (PITEM (car p) (cdr p)))
      (hash->list combine_hash)))))

;; (check-equal? (_galios-poly-add "9" "9") "")
