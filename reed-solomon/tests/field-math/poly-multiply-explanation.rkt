#lang racket

(require "../../src/field-math.rkt")

;; explain each step of poly multiply, include common mode and galios mode
;; common mode: same index's item's coefficients use add.
;; galios mode: same index's item's coefficients use xor.

(define (poly-multiply-explanation poly_multiplicand poly_multiplier [is_galios? #f])
  (printf "Poly Multiply Explanation\n\n")
  
  (printf "multiplicand poly: ~a\n\n" poly_multiplicand)

  (printf "multiplier poly: ~a\n\n" poly_multiplier)

  (printf "is_galios?: ~a\n\n" is_galios?)

  (let ([poly_multiplicand_pairs (poly->index_coe_pairs poly_multiplicand)]
        [poly_multiplier_pairs (poly->index_coe_pairs poly_multiplier)])
    
    (printf "poly_multiplicand_pairs:~a\n\n" poly_multiplicand_pairs)
    (printf "poly_multiplier_pairs:~a\n\n" poly_multiplier_pairs)
    
    (let loop-multiplicand ([loop_poly_multiplicand_pairs poly_multiplicand_pairs]
                            [multiplicand_list '()])
      (printf "multiplicand_list:~a\n\n" multiplicand_list)
      (if (not (null? loop_poly_multiplicand_pairs))
          (loop-multiplicand
           (cdr loop_poly_multiplicand_pairs)
           (cons
            (index_coe_pairs->poly
             (let loop-multiplier ([loop_poly_multiplier_pairs poly_multiplier_pairs]
                                   [multiplier_list '()])
               (if (not (null? loop_poly_multiplier_pairs))
                   (loop-multiplier
                    (cdr loop_poly_multiplier_pairs)
                    (cons
                     (cons
                      (+ (caar loop_poly_multiplicand_pairs) (caar loop_poly_multiplier_pairs))
                      (* (cdar loop_poly_multiplicand_pairs) (cdar loop_poly_multiplier_pairs)))
                     multiplier_list))
                   (reverse multiplier_list))))
            multiplicand_list))
          (index_coe_pairs->poly
           (let ([combine_hash (make-hash)])
             (let loop ([loop_polys multiplicand_list])
               (when (not (null? loop_polys))
                 (map
                  (lambda (p)
                    (if (hash-has-key? combine_hash (car p))
                        (if is_galios?
                            (let ([coe_bitwised (bitwise-xor (cdr p) (hash-ref combine_hash (car p)))])
                              (if (= coe_bitwised 0)
                                  (hash-remove! combine_hash (car p))
                                  (hash-set! combine_hash (car p) coe_bitwised)))
                            (hash-set! combine_hash (car p) (+ (hash-ref combine_hash (car p)) (cdr p))))
                        (hash-set! combine_hash (car p) (cdr p))))
                  (poly->index_coe_pairs (car loop_polys)))
                 (loop (cdr loop_polys))))
             (hash->list combine_hash)))))))

(poly-multiply-explanation "x2+3x+2" "x+3") ;; "x3+7x2+14x+8")
