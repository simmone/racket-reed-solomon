#lang racket

(require rackunit/text-ui)

(require "../../lib/share/gf.rkt")

(require rackunit "../../lib/decode/chien-search.rkt")

(define test-chien-search
  (test-suite 
   "test-chien-search"

   (test-case
    "test-chien-value"

    (parameterize*
     ([*bit_width* 4]
      [*2^m_1* (sub1 (expt 2 (*bit_width*)))]
      [*primitive_poly_value* 19]
      [*gf_aton_map* (get-gf-aton-hash)]
      [*gf_ntoa_map* (make-hash (hash-map (*gf_aton_map*) (lambda (a n) (cons n a))))])
     
     (check-equal? (chien-value "14x2+14x1+1x0" 14) 3)
     (check-equal? (chien-value "14x2+14x1+1x0" 13) 13)
     (check-equal? (chien-value "14x2+14x1+1x0" 12) 12)
     (check-equal? (chien-value "14x2+14x1+1x0" 11) 3)
     (check-equal? (chien-value "14x2+14x1+1x0" 10) 15)
     (check-equal? (chien-value "14x2+14x1+1x0" 9) 0)
     (check-equal? (chien-value "14x2+14x1+1x0" 8) 14)
     (check-equal? (chien-value "14x2+14x1+1x0" 7) 13)
     (check-equal? (chien-value "14x2+14x1+1x0" 6) 14)
     (check-equal? (chien-value "14x2+14x1+1x0" 5) 15)
     (check-equal? (chien-value "14x2+14x1+1x0" 4) 2)
     (check-equal? (chien-value "14x2+14x1+1x0" 3) 2)
     (check-equal? (chien-value "14x2+14x1+1x0" 2) 0)
     (check-equal? (chien-value "14x2+14x1+1x0" 1) 12)
     (check-equal? (chien-value "14x2+14x1+1x0" 0) 1)
     
     (check-equal? (chien-search "14x2+14x1+1x0") '(9 2))
    ))
   ))

(run-tests test-chien-search)
