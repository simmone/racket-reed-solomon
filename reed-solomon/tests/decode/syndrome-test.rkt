#lang racket

(require rackunit/text-ui rackunit)

(require "../../src/lib/gf.rkt")
(require "../../src/decode/syndrome.rkt")

(define test-syndrome
  (test-suite 
   "test-syndrome"

   (test-case
    "test-get-syndromes"

    (parameterize*
     ([*bit_width* 4]
      [*2^m_1* (sub1 (expt 2 (*bit_width*)))]
      [*primitive_poly_value* 19]
      [*gf_aton_map* (get-gf-aton-hash)]
      [*gf_ntoa_map* (make-hash (hash-map (*gf_aton_map*) (lambda (a n) (cons n a))))])

     (check-equal? (get-syndromes 
                    '(1 2 3 4 5 11 7 8 9 10 11 3 1 12 12)
                    4)
                   '(12 4 3 15))))

   (test-case
    "test-get-syndromes"

    (parameterize*
     ([*bit_width* 8]
      [*2^m_1* (sub1 (expt 2 (*bit_width*)))]
      [*primitive_poly_value* 285]
      [*gf_aton_map* (get-gf-aton-hash)]
      [*gf_ntoa_map* (make-hash (hash-map (*gf_aton_map*) (lambda (a n) (cons n a))))])

     (check-equal? (get-syndromes
                    '(32 91 10 121 209 114 220 77 67 64 236 16 235 17 236 17 196 35 39 119 235 215 231 226 93 22)
                    10)
                   '(127 213 228 134 89 149 113 122 131 7))))
    ))

(run-tests test-syndrome)
