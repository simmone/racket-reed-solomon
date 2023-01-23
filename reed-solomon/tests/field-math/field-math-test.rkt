#lang racket

(require rackunit/text-ui rackunit)

(require "../../src/field-math.rkt")

(define test-field-math
  (test-suite 
   "test-field-math"
   
   (test-case
    "galios-a->n-map"

    ;; (15, 11)
    (let ([aton_map (get-galios-a->n_map 4 "x4+x1+1")])
      (check-equal? (hash-ref aton_map "0") 0)
      (check-equal? (hash-ref aton_map "a0") 1)
      (check-equal? (hash-ref aton_map "a1") 2)
      (check-equal? (hash-ref aton_map "a2") 4)
      (check-equal? (hash-ref aton_map "a3") 8)
      (check-equal? (hash-ref aton_map "a4") 3)
      (check-equal? (hash-ref aton_map "a5") 6)
      (check-equal? (hash-ref aton_map "a6") 12)
      (check-equal? (hash-ref aton_map "a7") 11)
      (check-equal? (hash-ref aton_map "a8") 5)
      (check-equal? (hash-ref aton_map "a9") 10)
      (check-equal? (hash-ref aton_map "a10") 7)
      (check-equal? (hash-ref aton_map "a11") 14)
      (check-equal? (hash-ref aton_map "a12") 15)
      (check-equal? (hash-ref aton_map "a13") 13)
      (check-equal? (hash-ref aton_map "a14") 9)
      )

    ;; (255, 239)
    (let ([aton_map (get-galios-a->n_map 8 "x8+x4+x3+x2+1")])
      (check-equal? (hash-ref aton_map "0") 0)
      (check-equal? (hash-ref aton_map "a0") 1)
      (check-equal? (hash-ref aton_map "a1") 2)
      (check-equal? (hash-ref aton_map "a2") 4)
      (check-equal? (hash-ref aton_map "a3") 8)
      (check-equal? (hash-ref aton_map "a4") 16)
      (check-equal? (hash-ref aton_map "a5") 32)
      (check-equal? (hash-ref aton_map "a6") 64)
      (check-equal? (hash-ref aton_map "a7") 128)
      (check-equal? (hash-ref aton_map "a8") 29)
      (check-equal? (hash-ref aton_map "a9") 58)
      (check-equal? (hash-ref aton_map "a69") 47)
      (check-equal? (hash-ref aton_map "a79") 240)
      (check-equal? (hash-ref aton_map "a204") 221)
      (check-equal? (hash-ref aton_map "a254") 142)
      )
    )
   
   (test-case
    "poly->index_coe_pairs && index_coe_pairs->poly"
    
    (check-equal? (poly->index_coe_pairs "1") '((0 . 1)))
    (check-equal? (index_coe_pairs->poly '((0 . 1))) "1")

    (check-equal? (poly->index_coe_pairs "4") '((0 . 4)))
    (check-equal? (index_coe_pairs->poly '((0 . 4))) "4")

    (check-equal? (poly->index_coe_pairs "x") '((1 . 1)))
    (check-equal? (poly->index_coe_pairs "x1") '((1 . 1)))
    (check-equal? (index_coe_pairs->poly '((1 . 1))) "x")

    (check-equal? (poly->index_coe_pairs "2x") '((1 . 2)))
    (check-equal? (index_coe_pairs->poly '((1 . 2))) "2x")

    (check-equal? (poly->index_coe_pairs "x4+x3+x2+x+1") '((4 . 1) (3 . 1) (2 . 1) (1 . 1) (0 . 1)))
    (check-equal? (poly->index_coe_pairs " x4 + x3+x2+x1+1 ") '((4 . 1) (3 . 1) (2 . 1) (1 . 1) (0 . 1)))
    (check-equal? (poly->index_coe_pairs "x4+x3+x1+x2+1") '((4 . 1) (3 . 1) (2 . 1) (1 . 1) (0 . 1)))
    (check-equal? (index_coe_pairs->poly '((4 . 1) (3 . 1) (2 . 1) (1 . 1) (0 . 1))) "x4+x3+x2+x+1")

    (check-equal? (index_coe_pairs->poly '((4 . 1) (3 . 1) (1 . 1) (2 . 1) (0 . 1))) "x4+x3+x2+x+1")
    )

   (test-case
    "poly-multiply"
    
    (check-equal? (poly-multiply "x2+x1+1" "x") "x3+x2+x")
    (check-equal? (poly-multiply "x2+x1+1" "x2") "x4+x3+x2")
    (check-equal? (poly-multiply "x+1" "x+2" "x+3" "x+4") "x4+15x3+3x2+x+12")
    )
   
   (test-case
    "poly-galios-multiply"
    
    (check-equal? (galios-poly-multiply "x3+x" "x3+x2+1") "x6+x5+x4+x")
    (check-equal? (galios-poly-multiply "x4+x2+x+1" "x3+x4") "x8+x7+x6+x3")
    )
   
   (test-case
    "poly-sum"
    
    (check-equal? (poly-sum "1") 1)
    (check-equal? (poly-sum "x1") 2)
    (check-equal? (poly-sum "x1+1") 3)
    (check-equal? (poly-sum "x2") 4)
    (check-equal? (poly-sum "x2+1") 5)
    (check-equal? (poly-sum "x2+x1") 6)
    (check-equal? (poly-sum "x2+x1+1") 7)

    (check-equal? (poly-sum "4") 4)
    (check-equal? (poly-sum "2x") 4)
    (check-equal? (poly-sum "4x+1") 9)
    (check-equal? (poly-sum "3x2+2x1+1") 17)
    )
   
   (test-case
    "poly-remove_dup"
    
    (check-equal? (poly-remove_dup "x4+x+1+x") "x4+1")
    (check-equal? (poly-remove_dup "x4+x+1+x+1") "x4")
    (check-equal? (poly-remove_dup "x4+x2+1+x+1") "x4+x2+x")
    )
   
   (test-case
    "poly->coefficients"
    
    (check-equal? (poly->coefficients "x6+x5+x4+x1") "1110010")
    (check-equal? (poly->coefficients "x4+x1+1") "10011")
    )

  ))

(run-tests test-field-math)
