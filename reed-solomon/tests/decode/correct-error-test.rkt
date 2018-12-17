#lang racket

(require rackunit/text-ui)

(require "../../lib/share/gf.rkt")

(require rackunit "../../lib/decode/correct-error.rkt")

(define test-correct-error
  (test-suite
   "test-correct-error"

   (test-case
    "test-correct-error"

    (parameterize*
     ([*bit_width* 4]
      [*2^m_1* (sub1 (expt 2 (*bit_width*)))]
      [*primitive_poly_value* 19]
      [*gf_aton_map* (get-gf-aton-hash)]
      [*gf_ntoa_map* (make-hash (hash-map (*gf_aton_map*) (lambda (a n) (cons n a))))])

     (check-equal? (correct-error 
                    '(1 2 3 4 5 11 7 8 9 10 11 3 1 12 12)
                    '( (9 . 13) (2 . 2) ))
                   '(1 2 3 4 5 6 7 8 9 10 11 3 3 12 12))
    ))

   ))

(run-tests test-correct-error)
