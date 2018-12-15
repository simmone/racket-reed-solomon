#lang racket

(require rackunit/text-ui)

(require "../../lib/share/gf.rkt")

(require rackunit "../../lib/share/euclidean.rkt")

(define test-euclidean
  (test-suite 
   "test-euclidean"
   
   (test-case
    "test-euc-divide"

    (parameterize*
     ([*bit_width* 4]
      [*2^m_1* (sub1 (expt 2 (*bit_width*)))]
      [*primitive_poly_value* 19]
      [*gf_aton_map* (get-gf-aton-hash)]
      [*gf_ntoa_map* (make-hash (hash-map (*gf_aton_map*) (lambda (a n) (cons n a))))])

     (let-values ([(quotient remainder) (euc-divide "x4" "12x3+4x2+3x+15")])
       (check-equal? quotient "10x1+6x0")
       (check-equal? remainder "6x2+6x1+4x0"))

     (let-values ([(quotient remainder) (euc-divide "12x3+4x2+3x+15" "6x2+6x1+4x0")])
       (check-equal? quotient "2x1+13x0")
       (check-equal? remainder "3x1+14x0"))

     ))

;   (test-case
;    "test-euc-divide"
;
;    (parameterize*
;     ([*bit_width* 4]
;      [*2^m_1* (sub1 (expt 2 (*bit_width*)))]
;      [*primitive_poly_value* 19]
;      [*gf_aton_map* (get-gf-aton-hash)]
;      [*gf_ntoa_map* (make-hash (hash-map (*gf_aton_map*) (lambda (a n) (cons n a))))])
;
;     (check-equal? (euc-divide "12x3+4x2+3x+15" "6x2+6x1+4x0" 2)
;                   (cons "6x+15" "14x2+14x+1x0"))
;    ))
;
;   (test-case
;    "test-get-euclideans"
;
;    (parameterize*
;     ([*bit_width* 4]
;      [*2^m_1* (sub1 (expt 2 (*bit_width*)))]
;      [*primitive_poly_value* 19]
;      [*gf_aton_map* (get-gf-aton-hash)]
;      [*gf_ntoa_map* (make-hash (hash-map (*gf_aton_map*) (lambda (a n) (cons n a))))])
;
;     (check-equal? (euclideans 
;                    '(1 2 3 4 5 11 7 8 9 10 11 3 1 12 12)
;                    2)
;                   "14x2+14x+1")
;     ))

    ))

(run-tests test-euclidean)