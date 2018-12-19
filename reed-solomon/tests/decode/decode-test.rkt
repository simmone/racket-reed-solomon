#lang racket

(require rackunit/text-ui)

(require "../../lib/share/gf.rkt")

(require rackunit "../../lib/decode/decode.rkt")

(define test-decode
  (test-suite
   "test-decode"

   (test-case
    "test-decode-4"

    ;; fix two errors
    (check-equal? (rs-decode 
                   '(1 2 3 4 5 11 7 8 9 10 11 3 1 12 12) 4
                   #:bit_width 4 #:primitive_poly_value 19)
                  '(1 2 3 4 5 6 7 8 9 10 11 3 3 12 12))

    ;; fix one errors
    (check-equal? (rs-decode 
                   '(1 2 3 4 5 6 7 8 9 10 11 3 1 12 12) 4
                   #:bit_width 4 #:primitive_poly_value 19)
                  '(1 2 3 4 5 6 7 8 9 10 11 3 3 12 12))

    ;; no error
    (check-equal? (rs-decode 
                   '(1 2 3 4 5 6 7 8 9 10 11 3 3 12 12) 4
                   #:bit_width 4 #:primitive_poly_value 19)
                  '(1 2 3 4 5 6 7 8 9 10 11 3 3 12 12))

    ;; three errors
    (check-equal? (rs-decode 
                   '(5 2 3 4 5 11 7 8 9 10 11 3 1 12 12) 4
                   #:bit_width 4 #:primitive_poly_value 19)
                   '(5 2 3 4 5 11 7 8 9 10 11 3 1 12 12))

    ;; all is errors
    (check-equal? (rs-decode 
                   '(12 12 1 3 11 10 9 8 7 6 5 4 3 2 1) 4
                   #:bit_width 4 #:primitive_poly_value 19 #:express? #t)
                   '(12 12 1 3 11 10 9 8 7 6 5 4 3 2 1))

    )

   ))

(run-tests test-decode)
