#lang racket

(require rackunit/text-ui)

(require "../../lib/share/gf.rkt")

(require rackunit "../../lib/decode/euclidean.rkt")

(define test-euclidean
  (test-suite 
   "test-euclidean"

   (test-case
    "test-get-euclideans"

    (parameterize*
     ([*bit_width* 4]
      [*2^m_1* (sub1 (expt 2 (*bit_width*)))]
      [*primitive_poly_value* 19]
      [*gf_aton_map* (get-gf-aton-hash)]
      [*gf_ntoa_map* (make-hash (hash-map (*gf_aton_map*) (lambda (a n) (cons n a))))])

     (check-equal? (euclideans 
                    '(1 2 3 4 5 11 7 8 9 10 11 3 1 12 12)
                    4)
                   '(12 4 3 15))))
    ))

(run-tests test-euclidean)
