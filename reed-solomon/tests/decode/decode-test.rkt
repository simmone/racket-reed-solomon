#lang racket

(require rackunit/text-ui)

(require "../../lib/share/gf.rkt")

(require rackunit "../../decode.rkt")

(define test-decode
  (test-suite
   "test-decode"

   (test-case
    "test-decode"

    (check-equal? (decode '(1 2 3 4 5 11 7 8 9 10 11 3 1 12 12) 4 #:bit_width 4 #:primitive_poly_value 19 #:express? #t)
                  '(1 2 3 4 5 6 7 8 9 10 11 3 3 12 12))
    )

   ))

(run-tests test-decode)
