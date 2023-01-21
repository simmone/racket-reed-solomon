#lang racket

(require rackunit/text-ui rackunit)

(require "../../src/field-math.rkt")

(define test-field-math
  (test-suite 
   "test-field-math"
   
   (test-case
    "poly->num_index_pairs && num_index_pairs->poly"
    
    (check-equal? (poly->num_index_pairs "1") '((1 . 0)))
    (check-equal? (num_index_pairs->poly '((1 . 0))) "1")

    (check-equal? (poly->num_index_pairs "x") '((1 . 1)))
    (check-equal? (poly->num_index_pairs "x1") '((1 . 1)))
    (check-equal? (num_index_pairs->poly '((1 . 1))) "x")

    (check-equal? (poly->num_index_pairs "x4+x3+x2+x+1") '((1 . 4) (1 . 3) (1 . 2) (1 . 1) (1 . 0)))
    (check-equal? (poly->num_index_pairs " x4 + x3+x2+x1+1 ") '((1 . 4) (1 . 3) (1 . 2) (1 . 1) (1 . 0)))
    (check-equal? (poly->num_index_pairs "x4+x3+x1+x2+1") '((1 . 4) (1 . 3) (1 . 2) (1 . 1) (1 . 0)))
    (check-equal? (num_index_pairs->poly '((1 . 4) (1 . 3) (1 . 2) (1 . 1) (1 . 0))) "x4+x3+x2+x+1")

    (check-equal? (num_index_pairs->poly '((1 . 4) (1 . 3) (1 . 1) (1 . 2) (1 . 0))) "x4+x3+x2+x+1")
    )

   (test-case
    "poly-multiply"
    
    (check-equal? (poly-multiply "x2+x1+1" "x") "x3+x2+x")
    (check-equal? (poly-multiply "x2+x1+1" "x2") "x4+x3+x2")
    )

   (test-case
    "poly-multiply-poly"
    
    (check-equal? (poly-multiply-poly "x3+x" "x3+x2+1") "x6+x5+x4+x1")
    (check-equal? (poly-multiply-poly "x4+x2+x+1" "x3+x4") "x8+x7+x6+x3")
    )
   
   (test-case
    "poly->sum"
    
    (check-equal? (poly->sum "1") 1)
    (check-equal? (poly->sum "x1") 2)
    (check-equal? (poly->sum "x1+1") 3)
    (check-equal? (poly->sum "x2") 4)
    (check-equal? (poly->sum "x2+1") 5)
    (check-equal? (poly->sum "x2+x1") 6)
    (check-equal? (poly->sum "x2+x1+1") 7)
    )
   
   (test-case
    "poly->equal_pair"
    
    (check-equal? (poly->equal_pair "x4+x+1") '("x4" . "x1+1"))
    (check-equal? (poly->equal_pair "x8+x4+x3+x2+1") '("x8" . "x4+x3+x2+1"))
    )
   
   (test-case
    "poly-remove_dup"
    
    (check-equal? (poly-remove_dup "x4+x+1+x") "x4+1")
    (check-equal? (poly-remove_dup "x4+x+1+x+1") "x4")
    (check-equal? (poly-remove_dup "x4+x2+1+x+1") "x4+x2+x1")
    )
   
   (test-case
    "poly->coefficients"
    
    (check-equal? (poly->coefficients "x6+x5+x4+x1") "1110010")
    (check-equal? (poly->coefficients "x4+x1+1") "10011")
    )

  ))

(run-tests test-field-math)
