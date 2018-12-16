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

     (let-values ([(quotient remainder) (euc-divide "7x2+7x+9" "9")])
       (check-equal? quotient "14x2+14x1+1x0")
       (check-equal? remainder ""))

     (let-values ([(quotient remainder) (euc-divide "3x1+14x0" "9")])
       (check-equal? quotient "6x1+15x0")
       (check-equal? remainder ""))

    ))
   
   ))

(run-tests test-euclidean)
