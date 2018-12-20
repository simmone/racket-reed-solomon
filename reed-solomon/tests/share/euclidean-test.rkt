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

     (let-values ([(quotient remainder) (euc-divide "15x9+6" "14")])
       (check-equal? quotient "2x9+10x0")
       (check-equal? remainder ""))

     (let-values ([(quotient remainder) (euc-divide "15x2+6" "14")])
       (check-equal? quotient "2x2+10x0")
       (check-equal? remainder ""))

     (let-values ([(quotient remainder) (euc-divide "6x+15" "14")])
       (check-equal? quotient "10x1+2x0")
       (check-equal? remainder ""))
     )

    (parameterize*
     ([*bit_width* 8]
      [*2^m_1* (sub1 (expt 2 (*bit_width*)))]
      [*primitive_poly_value* 285]
      [*gf_aton_map* (get-gf-aton-hash)]
      [*gf_ntoa_map* (make-hash (hash-map (*gf_aton_map*) (lambda (a n) (cons n a))))])

     (let-values ([(quotient remainder) 
                   (euc-divide
                    "36x32+37x31+13x30+230x29+157x28+251x27+89x26+97x25+221x24+53x23+142x22+10x21+202x20+78x19+105x18+212x17+173x16+81x15+226x14+58x13+142x12+94x11+216x10+37x9+170x8+227x7+216x6+51x5+65x4+104x3+57x2+150x1+46x0"
                    "90x31+37x30+110x29+211x28+242x27+150x26+94x25+229x24+231x23+222x22+79x21+189x20+0x19+15x18+223x17+148x16+99x15+33x14+35x13+173x12+129x11+106x10+246x9+160x8+174x7+24x6+252x5+83x4+244x3+243x2+107x1+80x0"
                   #t)
                   ])
       (check-equal? quotient "10x1+2x0")
       (check-equal? remainder ""))
    )

    )
   
   ))

(run-tests test-euclidean)
