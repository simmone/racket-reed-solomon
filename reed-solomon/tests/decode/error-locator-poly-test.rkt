#lang racket

(require rackunit/text-ui rackunit)

(require "../../src/lib/gf.rkt")
(require "../../src/decode/error-locator-poly.rkt")

(define test-error-locator-poly
  (test-suite 
   "test-error-locator-poly"

   (test-case
    "test-get-error-locator-poly"

;    (parameterize*
;     ([*bit_width* 4]
;      [*2^m_1* (sub1 (expt 2 (*bit_width*)))]
;      [*primitive_poly_value* 19]
;      [*gf_aton_map* (get-gf-aton-hash)]
;      [*gf_ntoa_map* (make-hash (hash-map (*gf_aton_map*) (lambda (a n) (cons n a))))])
;
;     (hash-set! (*gf_ntoa_map*) 0 0)
;     
;     (let-values ([(ome lam) (error-locator-poly "12x3+4x2+3x1+15" 2)])
;       (check-equal? ome "6x1+15x0")
;       (check-equal? lam "14x2+14x1+1x0"))
;    )
;
    (parameterize*
     ([*bit_width* 8]
      [*2^m_1* (sub1 (expt 2 (*bit_width*)))]
      [*primitive_poly_value* 285]
      [*gf_aton_map* (get-gf-aton-hash)]
      [*gf_ntoa_map* (make-hash (hash-map (*gf_aton_map*) (lambda (a n) (cons n a))))])

     (hash-set! (*gf_ntoa_map*) 0 0)

     (let-values ([(ome lam) (error-locator-poly "208x9+221x8+122x7+7x6+253x5+253x4+76x3+219x2+69x1+7x0" 5)])
       (check-equal? ome "29x4+161x3+50x2+49x1+7x0")
       (check-equal? lam "141x5+214x4+47x3+252x2+194x1+1x0")
    )

    ))


   ))

(run-tests test-error-locator-poly)
