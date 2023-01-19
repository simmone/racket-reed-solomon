#lang racket

(require rackunit/text-ui rackunit)

(require "../../src/field-math.rkt")

(define test-field-math
  (test-suite 
   "test-field-math"
   
   (test-case
    "poly->indexes"
    
    (check-equal? (poly->indexes "1") '(0))
    (check-equal? (poly->indexes "a") '(1))
    (check-equal? (poly->indexes "a4+a3+a2+a1+1") '(4 3 2 1 0))
    (check-equal? (poly->indexes " a4 + a3+a2+a1+1 ") '(4 3 2 1 0))
    (check-equal? (poly->indexes "a4+a3+a1+a2+1") '(4 3 2 1 0))
    )

   (test-case
    "indexes->poly"
    
    (check-equal? (indexes->poly '(0)) "1")
    (check-equal? (indexes->poly '(4 3 2 1 0)) "a4+a3+a2+a1+1")
    (check-equal? (indexes->poly '(4 2 3 1 0)) "a4+a3+a2+a1+1")
    )

   (test-case
    "poly-multiply-n"
    
    (check-equal? (poly-multiply-n "a2+a1+1" 1) "a3+a2+a1")
    (check-equal? (poly-multiply-n "a2+a1+1" 2) "a4+a3+a2")
    )

   (test-case
    "poly-multiply-poly"
    
    (check-equal? (poly-multiply-poly "a3+a" "a3+a2+1") "a6+a5+a4+a1")
    (check-equal? (poly-multiply-poly "a4+a2+a+1" "a3+a4") "a8+a7+a6+a3")
    )
   
   (test-case
    "poly->sum"
    
    (check-equal? (poly->sum "1") 1)
    (check-equal? (poly->sum "a1") 2)
    (check-equal? (poly->sum "a1+1") 3)
    (check-equal? (poly->sum "a2") 4)
    (check-equal? (poly->sum "a2+1") 5)
    (check-equal? (poly->sum "a2+a1") 6)
    (check-equal? (poly->sum "a2+a1+1") 7)
    )
   
   (test-case
    "poly->equal_pair"
    
    (check-equal? (poly->equal_pair "a4+a+1") '("a4" . "a1+1"))
    (check-equal? (poly->equal_pair "a8+a4+a3+a2+1") '("a8" . "a4+a3+a2+1"))
    )
   
   (test-case
    "poly-remove_dup"
    
    (check-equal? (poly-remove_dup "a4+a+1+a") "a4+1")
    (check-equal? (poly-remove_dup "a4+a+1+a+1") "a4")
    (check-equal? (poly-remove_dup "a4+a2+1+a+1") "a4+a2+a1")
    )
   
   (test-case
    "poly->coefficients"
    
    (check-equal? (poly->coefficients "a6+a5+a4+a1") '(1 1 1 0 0 1 0))
    (check-equal? (poly->coefficients "a4+a1+1") '(1 0 0 1 1)))

  ))

(run-tests test-field-math)
