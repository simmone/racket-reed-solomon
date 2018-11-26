#lang racket

(require rackunit/text-ui)

(require rackunit "../lib/poly.rkt")

(define test-poly
  (test-suite 
   "test-poly"

   (test-case
    "test-poly-multiply"
    
    (check-equal? (poly-multiply "a0x1+a0x0" "a0x1+a1x0") "a0x1+a0x2+a1x0+a1x1")

    )

   ))

(run-tests test-poly)
