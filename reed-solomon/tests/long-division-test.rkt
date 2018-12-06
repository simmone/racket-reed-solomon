#lang racket

(require rackunit/text-ui)

(require "../lib/poly.rkt")

(require rackunit "../lib/long-division.rkt")

(define test-long-division
  (test-suite 
   "test-long-division"

   (test-case
    "test-message->poly"

    (check-equal? (message->poly '(72 69 76 76 79 32 87 79 82 76 68))
                  "72x10+69x9+76x8+76x7+79x6+32x5+87x4+79x3+82x2+76x1+68x0")
    
    (check-equal? (message->poly '(32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17))
                  "32x15+91x14+11x13+120x12+209x11+114x10+220x9+77x8+67x7+64x6+236x5+17x4+236x3+17x2+236x1+17x0")
    )
   
   (test-case
    "test-prepare-message"

    (parameterize*
     ([*bit_width* 8]
      [*2^m_1* (sub1 (expt 2 (*bit_width*)))]
      [*primitive_poly_value* 285]
      [*gf_aton_map* (get-gf-aton-hash)]
      [*gf_ntoa_map* (make-hash (hash-map (*gf_aton_map*) (lambda (a n) (cons n a))))])

     (check-equal?
      (prepare-message
       "32x15+91x14+11x13+120x12+209x11+114x10+220x9+77x8+67x7+64x6+236x5+17x4+236x3+17x2+236x1+17x0"
        10)
      "32x25+91x24+11x23+120x22+209x21+114x20+220x19+77x18+67x17+64x16+236x15+17x14+236x13+17x12+236x11+17x10+0x9+0x8+0x7+0x6+0x5+0x4+0x3+0x2+0x1+0x0")
     ))

   
   (test-case
    "test-prepare-generator"

    (parameterize*
     ([*bit_width* 8]
      [*2^m_1* (sub1 (expt 2 (*bit_width*)))]
      [*primitive_poly_value* 285]
      [*gf_aton_map* (get-gf-aton-hash)]
      [*gf_ntoa_map* (make-hash (hash-map (*gf_aton_map*) (lambda (a n) (cons n a))))])
    
     (check-equal?
      (prepare-generator
       "x10+a251x9+a67x8+a46x7+a61x6+a118x5+a70x4+a64x3+a94x2+a32x+a45"
       25)
      "a0x25+a251x24+a67x23+a46x22+a61x21+a118x20+a70x19+a64x18+a94x17+a32x16+a45x15")
     ))
   
   (test-case
    "test-poly-n-xor"
    
    (check-equal? (poly-n-xor "9x3+10x2" "30x3") "23x3+10x2")

    (check-equal? (poly-n-xor "30x3" "9x3+10x2") "23x3+10x2")
    
    (check-equal? (poly-n-xor
                   "32x25+91x24+11x23+120x22+209x21+114x20+220x19+77x18+67x17+64x16+236x15+17x14+236x13+17x12+236x11+17x10"
                   "32x25+2x24+101x23+10x22+97x21+197x20+15x19+47x18+134x17+74x16+5x15")
                  "0x25+89x24+110x23+114x22+176x21+183x20+211x19+98x18+197x17+10x16+233x15+17x14+236x13+17x12+236x11+17x10")
    )
   ))

(run-tests test-long-division)
