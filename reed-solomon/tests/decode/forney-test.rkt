#lang racket

(require rackunit/text-ui)

(require "../../lib/share/gf.rkt")

(require rackunit "../../lib/decode/forney.rkt")

(define test-forney
  (test-suite 
   "test-forney"

   (test-case
    "test-derivative-forney"

    (parameterize*
     ([*bit_width* 4]
      [*2^m_1* (sub1 (expt 2 (*bit_width*)))]
      [*primitive_poly_value* 19]
      [*gf_aton_map* (get-gf-aton-hash)]
      [*gf_ntoa_map* (make-hash (hash-map (*gf_aton_map*) (lambda (a n) (cons n a))))])

     (check-equal? (derivative-lam "14x2+14x1+1x0") "14x0")
    ))

   (test-case
    "test-forney"

    (parameterize*
     ([*bit_width* 4]
      [*2^m_1* (sub1 (expt 2 (*bit_width*)))]
      [*primitive_poly_value* 19]
      [*gf_aton_map* (get-gf-aton-hash)]
      [*gf_ntoa_map* (make-hash (hash-map (*gf_aton_map*) (lambda (a n) (cons n a))))])

     (check-equal? (forney "14x2+14x1+1x0" "6x+15" 9) 13)

     (check-equal? (forney "14x2+14x1+1x0" "6x+15" 2) 2)
    ))

   ))

(run-tests test-forney)
