#lang racket

(require rackunit/text-ui rackunit)

(require "../../../src/lib/field-math.rkt")

(define test-field-math
  (test-suite 
   "test-field-math"
   
   (test-case
    "poly-a->index_list"
    
    (check-equal? (poly-a->index_list "1") '(0))
    (check-equal? (poly-a->index_list "a4+a3+a2+a1+1") '(4 3 2 1 0))
    (check-equal? (poly-a->index_list " a4 + a3+a2+a1+1 ") '(4 3 2 1 0))
    )

   (test-case
    "index_list->poly_a"
    
    (check-equal? (index_list->poly_a '(0)) "1")
    (check-equal? (index_list->poly_a '(4 3 2 1 0)) "a4+a3+a2+a1+1")
    (check-equal? (index_list->poly_a '(4 3 2 1 0)) " a4 + a3+a2+a1+1 ")
    )

   (test-case
    "poly-a-multiply-a"
    
    (check-equal? (poly-a-multiply-a "a2+1" "a") "a3+a")
    )

  ))

(run-tests test-field-math)
