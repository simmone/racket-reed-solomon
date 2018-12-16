#lang racket

(require rackunit/text-ui)

(require "../../lib/share/gf.rkt")

(require rackunit "../../lib/decode/error-locator-poly.rkt")

(define test-error-locator-poly
  (test-suite 
   "test-error-locator-poly"

   (test-case
    "test-get-error-locator-poly"

    (parameterize*
     ([*bit_width* 4]
      [*2^m_1* (sub1 (expt 2 (*bit_width*)))]
      [*primitive_poly_value* 19]
      [*gf_aton_map* (get-gf-aton-hash)]
      [*gf_ntoa_map* (make-hash (hash-map (*gf_aton_map*) (lambda (a n) (cons n a))))])
     
     (let-values ([(lam ome) (error-locator-poly "12x3+4x2+3x1+15" 2)])
       (check-equal? lam "14x2+14x1+1")
       (check-equal? ome "6x+15"))
    ))
   ))

(run-tests test-error-locator-poly)