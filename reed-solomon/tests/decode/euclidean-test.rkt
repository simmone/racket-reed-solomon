#lang racket

(require rackunit/text-ui)

(require "../../lib/share/gf.rkt")

(require rackunit "../../lib/decode/euclidean.rkt")

(define test-euclidean
  (test-suite 
   "test-euclidean"
   
   (test-case
    "test-init-t"
    
    (check-equal? (init-t 2) "x4")

    (check-equal? (init-t 2) "x4")
    )
   
   (test-case
    "test-euc-init"

    (parameterize*
     ([*bit_width* 4]
      [*2^m_1* (sub1 (expt 2 (*bit_width*)))]
      [*primitive_poly_value* 19]
      [*gf_aton_map* (get-gf-aton-hash)]
      [*gf_ntoa_map* (make-hash (hash-map (*gf_aton_map*) (lambda (a n) (cons n a))))])

     (check-equal? (euc-init "12x3+4x2+3x+15" 2)
                   (cons "6x2+6x1+4x0" "10x1+6x0"))
    ))

;   (test-case
;    "test-euc-divide-to-end"
;
;    (parameterize*
;     ([*bit_width* 4]
;      [*2^m_1* (sub1 (expt 2 (*bit_width*)))]
;      [*primitive_poly_value* 19]
;      [*gf_aton_map* (get-gf-aton-hash)]
;      [*gf_ntoa_map* (make-hash (hash-map (*gf_aton_map*) (lambda (a n) (cons n a))))])
;
;     (check-equal? (euc-divide-to-end "12x3+4x2+3x+15" "6x2+6x1+4x0" 2)
;                   (cons "3x+14" "7x2+7x+9x0"))
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
