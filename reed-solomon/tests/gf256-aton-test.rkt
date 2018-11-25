#lang racket

(require rackunit/text-ui)

(require rackunit "../lib/gf256.rkt")

(define test-gf256
  (test-suite 
   "test-gf256"

   (test-case
    "test-gf256-aton"

    (check-equal? (gf256-aton 0) 1)
    (check-equal? (gf256-aton 1) 2)
    (check-equal? (gf256-aton 2) 4)
    (check-equal? (gf256-aton 7) 128)
    (check-equal? (gf256-aton 8) 29)
    (check-equal? (gf256-aton 69) 47)
    (check-equal? (gf256-aton 204) 221)
    (check-equal? (gf256-aton 254) 142)
    (check-equal? (gf256-aton 255) 1)

    )

   ))

(run-tests test-gf256)
