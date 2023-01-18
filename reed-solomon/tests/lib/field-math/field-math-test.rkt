#lang racket

(require rackunit/text-ui rackunit)

(require "../../../src/lib/field-math.rkt")

(define test-field-math
  (test-suite 
   "test-field-math"
   
   (test-case
    "poly_a->index_list"
    
    (check-equal? (poly_a->index_list "1") '(0))
    (check-equal? (poly_a->index_list "a4+a3+a2+a1+1") '(4 3 2 1 0))
    (check-equal? (poly_a->index_list " a4 + a3+a2+a1+1 ") '(4 3 2 1 0))
    (check-equal? (poly_a->index_list "a4+a3+a1+a2+1") '(4 3 2 1 0))
    )

   (test-case
    "index_list->poly_a"
    
    (check-equal? (index_list->poly_a '(0)) "1")
    (check-equal? (index_list->poly_a '(4 3 2 1 0)) "a4+a3+a2+a1+1")
    (check-equal? (index_list->poly_a '(4 2 3 1 0)) "a4+a3+a2+a1+1")
    )

   (test-case
    "poly_a-multiply-n"
    
    (check-equal? (poly_a-multiply-n "a2+1" 1) "a3+a1")
    (check-equal? (poly_a-multiply-n "a2+1" 2) "a4+a2")
    )
   
   (test-case
    "poly_a->n"
    
    (check-equal? (poly_a->n "1") 1)
    (check-equal? (poly_a->n "a1") 2)
    (check-equal? (poly_a->n "a1+1") 3)
    (check-equal? (poly_a->n "a2") 4)
    (check-equal? (poly_a->n "a2+1") 5)
    (check-equal? (poly_a->n "a2+a1") 6)
    (check-equal? (poly_a->n "a2+a1+1") 7)
    )

  ))

(run-tests test-field-math)
