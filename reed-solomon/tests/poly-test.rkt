#lang racket

(require rackunit/text-ui)

(require rackunit "../lib/poly.rkt")

(define test-poly
  (test-suite 
   "test-poly"
   
   (test-case
    "test-string->poly"
    
    (check-equal? (string->poly "a0x1+a0x0") '( (0 . 1) (0 . 0) ))
    (check-equal? (string->poly "a0x1-a0x0") '( (0 . 1) (0 . 0) ))
    (check-equal? (string->poly "a1-x1") '( (1 . 0) (0 . 1) ))
    (check-equal? (string->poly "a1-a2") '( (1 . 0) (2 . 0) ))
    (check-equal? (string->poly "a1x7") '( (1 . 7) ))
    
    )
   
   (test-case
    "test-poly->string"

    (check-equal? (poly->string '( (0 . 1) (0 . 0) )) "a0x1+a0x0")
    (check-equal? (poly->string '( (0 . 1) (0 . 0) )) "a0x1+a0x0")
    (check-equal? (poly->string '( (1 . 0) (0 . 1) )) "a0x1+a1x0")
    (check-equal? (poly->string '( (1 . 0) (2 . 0) )) "a2x0+a1x0")
    (check-equal? (poly->string '( (1 . 7) )) "a1x7")
    (check-equal? (poly->string '( (1 . 0) (3 . 4) (5 . 2) (2 . 3) (3 . 3) (2 . 0) )) "a3x4+a3x3+a2x3+a5x2+a2x0+a1x0")
    )

   (test-case
    "test-poly-multiply"
    
    (check-equal? (poly-multiply "a0x1+a0x0" "a0x1+a1x0") "a0x2+a1x1+a0x1+a1x0")

    )

   ))

(run-tests test-poly)
