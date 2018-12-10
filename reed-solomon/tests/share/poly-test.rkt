#lang racket

(require rackunit/text-ui)

(require rackunit "../../lib/share/poly.rkt")

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
    (check-equal? (string->poly "a") '( (1 . 0) ))
    (check-equal? (string->poly "x") '( (0 . 1) ))
    (check-equal? (string->poly "") '( (0 . 0) ))
    (check-equal? (string->poly "a2") '( (2 . 0) ))
    (check-equal? (string->poly "x2") '( (0 . 2) ))
    (check-equal? (string->poly "ax") '( (1 . 1) ))
    (check-equal? (string->poly "1x2") '( (1 . 2) ))
    (check-equal? (string->poly "1") '( (1 . 0) ))
    
    )
   
   (test-case
    "test-poly->string"

    (check-equal? (poly->string '( (0 . 1) (0 . 0) )) "a0x1+a0x0")
    (check-equal? (poly->string '( (0 . 1) (0 . 0) )) "a0x1+a0x0")
    (check-equal? (poly->string '( (1 . 0) (0 . 1) )) "a0x1+a1x0")
    (check-equal? (poly->string '( (1 . 0) (2 . 0) )) "a2x0+a1x0")
    (check-equal? (poly->string '( (1 . 7) )) "a1x7")
    (check-equal? (poly->string '( (1 . 0) (3 . 4) (5 . 2) (2 . 3) (3 . 3) (2 . 0) )) "a3x4+a3x3+a2x3+a5x2+a2x0+a1x0")
    (check-equal? (poly->string '()) "")
    )

   (test-case
    "test-poly-combine-n"
    
    (check-equal? (poly-combine-n "3x1+2x1+8x0") "a1x1+a8x0")
    (check-equal? (poly-combine-n "3x2+2x1+0x1+8x0+5x0") "a3x2+a2x1+a13x0")

    (check-equal? (poly-combine-n "1x2+2x1+1x1+2x0") "a1x2+a3x1+a2x0")

    (check-equal? (poly-combine-n "1x3+4x2+3x2+12x1+2x1+8x0") "a1x3+a7x2+a14x1+a8x0")
    )
   
   ))

(run-tests test-poly)
