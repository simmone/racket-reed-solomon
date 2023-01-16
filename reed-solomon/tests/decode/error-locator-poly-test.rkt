#lang racket

(require rackunit/text-ui rackunit)

(require "../../src/lib/gf.rkt")
(require "../../src/decode/error-locator-poly.rkt")

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
     
     (let-values ([(ome lam) (error-locator-poly "12x3+4x2+3x1+15" 2)])
       (check-equal? ome "6x1+15x0")
       (check-equal? lam "14x2+14x1+1x0"))
    ))

   (test-case
    "test-get-error-locator-poly"

    (parameterize*
     ([*bit_width* 8]
      [*2^m_1* (sub1 (expt 2 (*bit_width*)))]
      [*primitive_poly_value* 285]
      [*gf_aton_map* (get-gf-aton-hash)]
      [*gf_ntoa_map* (make-hash (hash-map (*gf_aton_map*) (lambda (a n) (cons n a))))])
     
     (let-values ([(ome lam) (error-locator-poly "0x15+49x14+195x13+228x12+166x11+225x10+133x9+24x8+105x7+4x6+9x5+222x4+119x3+138x2+193x1+87x0" 8)])
       (check-equal? ome "6x1+15x0")
       (check-equal? lam "14x2+14x1+1x0"))
    ))

   ))

(run-tests test-error-locator-poly)
