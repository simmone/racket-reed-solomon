#lang racket

(require rackunit/text-ui rackunit)

(require "../../src/field-math.rkt")

(define test-field-math
  (test-suite 
   "test-field-math"

   (test-case
    "poly->items && items->poly"
    
    (check-true (check-pitem-list? (poly->items "1") (list (PITEM 0 1))))
    (check-equal? (items->poly (list (PITEM 0 1))) "1")

    (check-true (check-pitem-list? (poly->items "4") (list (PITEM 0 4))))
    (check-equal? (items->poly (list (PITEM 0 4))) "4")

    (check-true (check-pitem-list? (poly->items "x") (list (PITEM 1 1))))
    (check-true (check-pitem-list? (poly->items "x1") (list (PITEM 1 1))))
    (check-equal? (items->poly (list (PITEM 1 1))) "x")

    (check-true (check-pitem-list? (poly->items "2x") (list (PITEM 1 2))))
    (check-equal? (items->poly (list (PITEM 1 2))) "2x")

    (check-true (check-pitem-list? (poly->items "x4+x3+x2+x+1") (list (PITEM 4 1) (PITEM 3 1) (PITEM 2 1) (PITEM 1 1) (PITEM 0 1))))
    (check-true (check-pitem-list? (poly->items " x4 + x3+x2+x1+1 ") (list (PITEM 4 1) (PITEM 3 1) (PITEM 2 1) (PITEM 1 1) (PITEM 0 1))))
    (check-true (check-pitem-list? (poly->items "x4+x3+x1+x2+1") (list (PITEM 4 1) (PITEM 3 1) (PITEM 2 1) (PITEM 1 1) (PITEM 0 1))))
    (check-equal? (items->poly (list (PITEM 4 1) (PITEM 3 1) (PITEM 2 1) (PITEM 1 1) (PITEM 0 1))) "x4+x3+x2+x+1")

    (check-equal? (items->poly (list (PITEM 4 1) (PITEM 3 1) (PITEM 1 1) (PITEM 2 1) (PITEM 0 1))) "x4+x3+x2+x+1")
    )
   
   (test-case
    "number->binary_poly"
    
    (check-equal? (number->binary_poly 10) "x3+x")
    (check-equal? (number->binary_poly 13) "x3+x2+1")
    (check-equal? (number->binary_poly 12) "x3+x2")
    (check-equal? (number->binary_poly 1) "1")
    (check-equal? (number->binary_poly 2) "x"))

   (test-case
    "binary_poly->binary_string"
    
    (check-equal? (binary_poly->binary_string "x6+x5+x4+x1") "1110010")
    (check-equal? (binary_poly->binary_string "x4+x1+1") "10011")
    )

   (test-case
    "binary_string->binary_poly"
    
    (check-equal? (binary_string->binary_poly "1110010") "x6+x5+x4+x")
    (check-equal? (binary_string->binary_poly "10011") "x4+x+1")
    (check-equal? (binary_string->binary_poly "10") "x")
    )

   (test-case
    "galios-poly-add"

    (check-equal? (galios-poly-add "9" "9") "")
    (check-equal? (galios-poly-add "x+8") "x+8")
    (check-equal? (galios-poly-add "3x1" "2x1+8") "x+8")
    (check-equal? (galios-poly-add "3x2+2x+8" "5") "3x2+2x+13")
    (check-equal? (galios-poly-add "x2+2x" "1x+2") "x2+3x+2")
    (check-equal? (galios-poly-add "x3" "4x2" "3x2+12x" "2x+8") "x3+7x2+14x+8")
    (check-equal? (galios-poly-add "x4" "x4+14x3" "13x2+12x") "14x3+13x2+12x")
    (check-equal? (galios-poly-add "x4" "x4" "x3" "x3" "13x2+12x") "13x2+12x")
    (check-equal? (apply galios-poly-add '("x4" "x4" "x3" "x3" "13x2+12x")) "13x2+12x")
    (check-equal? (galios-poly-add "1x4" "x4+14x3+13x2+12x1") "14x3+13x2+12x")
    (check-equal? (galios-poly-add "x4" "x4+14x3+13x2+12x1") "14x3+13x2+12x")
    (check-equal? (galios-poly-add
                   "90x16+66x15+95x14+186x13+120x12+50x11+156x10+158x9+140x8+174x7+108x6+152x5+41x4+88x3+169x2+200x"
                   "90x16+66x15+197x14+102x13+133x12+118x11+150x10+226x9+234x8+103x7+89x6+63x5+165x4+56x3+242x2+250x+66")
                  "154x14+220x13+253x12+68x11+10x10+124x9+102x8+201x7+53x6+167x5+140x4+96x3+91x2+50x+66")
    )

   (test-case
    "binary_poly-multiply"
    
    (check-equal? (binary_poly-multiply "x3+x" "x3+x2+1") "x6+x5+x4+x")
    (check-equal? (binary_poly-multiply "1" "x") "x")
    )

   (test-case
    "binary_poly-divide"
    
    (check-equal? (binary_poly-divide "x6+x5+x4+x" "x4+x+1") "x3+x+1")
    (check-equal? (binary_poly-divide "x" "x4+x+1") "x")
    )
   
   (test-case
    "galios-num-multiply"
    
    (parameterize*
     ([*field_generator_poly* "x4+x+1"])

     (check-equal? (galios-num-multiply 10 12) 1)
     (check-equal? (galios-num-multiply 10 13) 11)
     (check-equal? (galios-num-multiply 14 14) 11)
     (check-equal? (galios-num-multiply 14 15) 5)
     (check-equal? (galios-num-multiply 3 6) 10)
     (check-equal? (galios-num-multiply 1 2) 2)
     (check-equal? (galios-num-multiply 0 0) 0)
     (check-equal? (galios-num-multiply 0 15) 0)
     (check-equal? (galios-num-multiply 15 0) 0)
    ))

   (test-case
    "galios-poly-multiply"

    (parameterize*
     ([*field_generator_poly* "x4+x+1"])

     (check-equal? (galios-poly-multiply "1" "x") "x")
     (check-equal? (galios-poly-multiply "x3+x" "x3+x2+1") "x6+x5+x4+x")
     (check-equal? (galios-poly-multiply "x2+x1+1" "x") "x3+x2+x")
     (check-equal? (galios-poly-multiply "x2+x1+1" "x2") "x4+x3+x2")
     (check-equal? (galios-poly-multiply "x+1" "x+2") "x2+3x+2")
     (check-equal? (galios-poly-multiply "x2+3x+2" "x+4") "x3+7x2+14x+8")
     (check-equal? (galios-poly-multiply "x3+7x2+14x+8" "x+8") "x4+15x3+3x2+x+12")
     (check-equal? (galios-poly-multiply "x3+x" "x3+x2+1") "x6+x5+x4+x")
     (check-equal? (galios-poly-multiply "x4+x2+x+1" "x3+x4") "x8+x7+x6+x3")
     (check-equal? (galios-poly-multiply "x+1" "x+2" "x+4" "x+8") "x4+15x3+3x2+x+12")
     (check-equal? (apply galios-poly-multiply '("x+1" "x+2" "x+4" "x+8")) "x4+15x3+3x2+x+12")
     ))
   
   (test-case
    "galios-poly-multiply-add-combine"

    (parameterize*
     ([*field_generator_poly* "x4+x+1"])

     (check-equal? (galios-poly-add "1" (galios-poly-multiply "10x+6" "2x+13")) "7x2+7x+9")
     ))
    

   (test-case
    "get-galios-index->number_map"

    ;; GF16
    (parameterize*
     ([*bit_width* 4]
      [*field_generator_poly* "x4+x+1"])

     (let ([aton_map (get-galios-index->number_map)])
       (check-equal? (hash-ref aton_map "0") 0)
       (check-equal? (hash-ref aton_map "a0") 1)
       (check-equal? (hash-ref aton_map "a1") 2)
       (check-equal? (hash-ref aton_map "a2") 4)
       (check-equal? (hash-ref aton_map "a3") 8)
       (check-equal? (hash-ref aton_map "a4") 3)
       (check-equal? (hash-ref aton_map "a5") 6)
       (check-equal? (hash-ref aton_map "a6") 12)
       (check-equal? (hash-ref aton_map "a7") 11)
       (check-equal? (hash-ref aton_map "a8") 5)
       (check-equal? (hash-ref aton_map "a9") 10)
       (check-equal? (hash-ref aton_map "a10") 7)
       (check-equal? (hash-ref aton_map "a11") 14)
       (check-equal? (hash-ref aton_map "a12") 15)
       (check-equal? (hash-ref aton_map "a13") 13)
       (check-equal? (hash-ref aton_map "a14") 9)
       ))

    ;; GF256
    (parameterize*
     ([*bit_width* 8]
      [*field_generator_poly* "x8+x4+x3+x2+1"])

     (let ([aton_map (get-galios-index->number_map)])
       (check-equal? (hash-ref aton_map "0") 0)
       (check-equal? (hash-ref aton_map "a0") 1)
       (check-equal? (hash-ref aton_map "a1") 2)
       (check-equal? (hash-ref aton_map "a2") 4)
       (check-equal? (hash-ref aton_map "a3") 8)
       (check-equal? (hash-ref aton_map "a4") 16)
       (check-equal? (hash-ref aton_map "a5") 32)
       (check-equal? (hash-ref aton_map "a6") 64)
       (check-equal? (hash-ref aton_map "a7") 128)
       (check-equal? (hash-ref aton_map "a8") 29)
       (check-equal? (hash-ref aton_map "a9") 58)
       (check-equal? (hash-ref aton_map "a69") 47)
       (check-equal? (hash-ref aton_map "a79") 240)
       (check-equal? (hash-ref aton_map "a204") 221)
       (check-equal? (hash-ref aton_map "a254") 142)
       ))
     )

   (test-case
    "get-code-generator-poly"
    
    (parameterize*
     ([*bit_width* 4]
      [*field_generator_poly* "x4+x+1"]
      [*galios_index->number_map* (get-galios-index->number_map)])

     (check-equal? (get-code-generator-poly 4) "x4+15x3+3x2+x+12"))

    (parameterize*
     ([*bit_width* 8]
      [*field_generator_poly* "x8+x4+x3+x2+1"]
      [*galios_index->number_map* (get-galios-index->number_map)])

     (check-equal? (get-code-generator-poly 16) "x16+59x15+13x14+104x13+189x12+68x11+209x10+30x9+8x8+163x7+65x6+41x5+229x4+98x3+50x2+36x+59"))
    )
   
   (test-case
    "poly-sum"
    
    (check-equal? (poly-sum "1") 1)
    (check-equal? (poly-sum "x1") 2)
    (check-equal? (poly-sum "x1+1") 3)
    (check-equal? (poly-sum "x2") 4)
    (check-equal? (poly-sum "x2+1") 5)
    (check-equal? (poly-sum "x2+x1") 6)
    (check-equal? (poly-sum "x2+x1+1") 7)

    (check-equal? (poly-sum "4") 4)
    (check-equal? (poly-sum "2x") 4)
    (check-equal? (poly-sum "4x+1") 9)
    (check-equal? (poly-sum "3x2+2x1+1") 17)
    )
   
   (test-case
    "poly-remove_dup"
    
    (check-equal? (poly-remove_dup "x4+x+1+x") "x4+1")
    (check-equal? (poly-remove_dup "x4+x+1+x+1") "x4")
    (check-equal? (poly-remove_dup "x4+x2+1+x+1") "x4+x2+x")
    )
   
   (test-case
    "galios-poly-divide-align"
    
    (parameterize*
     ([*bit_width* 4]
      [*field_generator_poly* "x4+x+1"]
      [*galios_index->number_map* (get-galios-index->number_map)]
      [*galios_number->index_map* (make-hash (hash-map (*galios_index->number_map*) (lambda (a n) (cons n a))))])

     (check-equal? (galios-poly-divide-align "x4" "12x3") "10x")

     (check-equal? (galios-poly-divide-align "14x3" "12x3") "6")

     (check-equal? (galios-poly-divide-align "12x3" "6x2") "2x")
    
     (check-equal? (galios-poly-divide-align "8x2" "6x2") "13")

     (check-equal? (galios-poly-divide-align "14x2" "7x2") "2"))

    (parameterize*
     ([*bit_width* 8]
      [*field_generator_poly* "x8+x4+x3+x2+1"]
      [*galios_index->number_map* (get-galios-index->number_map)]
      [*galios_number->index_map* (make-hash (hash-map (*galios_index->number_map*) (lambda (a n) (cons n a))))])

     (check-equal?
      (galios-poly-divide-align
       "x16"
       "49x14+195x13+228x12+166x11+225x10+133x9+24x8+105x7+4x6+9x5+222x4+119x3+138x2+193x1+87x0")
      "137x2"))
    )
   
   (test-case
    "galios-poly-divide"
    
    (parameterize*
     ([*bit_width* 4]
      [*field_generator_poly* "x4+x+1"]
      [*galios_index->number_map* (get-galios-index->number_map)]
      [*galios_number->index_map* (make-hash (hash-map (*galios_index->number_map*) (lambda (a n) (cons n a))))])

     (let-values ([(quotient remainder)
                   (galios-poly-divide "12x3+4x2+3x+15" "6x2+6x1+4")])
       (check-equal? quotient "2x+13")
       (check-equal? remainder "3x+14"))

     (let-values ([(quotient remainder) (galios-poly-divide "7x2+7x+9" "9")])
       (check-equal? quotient "14x2+14x+1")
       (check-equal? remainder ""))

     (let-values ([(quotient remainder) (galios-poly-divide "3x1+14" "9")])
       (check-equal? quotient "6x+15")
       (check-equal? remainder ""))

     (let-values ([(quotient remainder) (galios-poly-divide "15x9+6" "14")])
       (check-equal? quotient "2x9+10")
       (check-equal? remainder ""))

     (let-values ([(quotient remainder) (galios-poly-divide "15x2+6" "14")])
       (check-equal? quotient "2x2+10")
       (check-equal? remainder ""))

     (let-values ([(quotient remainder) (galios-poly-divide "6x+15" "14")])
       (check-equal? quotient "10x+2")
       (check-equal? remainder ""))
     )

    (parameterize*
     ([*bit_width* 8]
      [*field_generator_poly* "x8+x4+x3+x2+1"]
      [*galios_index->number_map* (get-galios-index->number_map)]
      [*galios_number->index_map* (make-hash (hash-map (*galios_index->number_map*) (lambda (a n) (cons n a))))])

     (let-values ([(quotient remainder) 
                   (galios-poly-divide
                    "11x33+94x32+132x31+202x30+153x29+38x28+98x27+136x26+183x25+101x24+175x23+127x22+122x21+33x20+121x19+118x18+133x17+96x16+6x15+94x14+173x13+232x12+200x11+48x10+3x9+219x8+224x7+239x6+216x5+107x4+66x3+151x2+44x1+6x0"
                    "36x32+37x31+13x30+230x29+157x28+251x27+89x26+97x25+221x24+53x23+142x22+10x21+202x20+78x19+105x18+212x17+173x16+81x15+226x14+58x13+142x12+94x11+216x10+37x9+170x8+227x7+216x6+51x5+65x4+104x3+57x2+150x1+46x0"
                    )
                   ])
       (check-equal? quotient "135x+225")
       (check-equal? remainder
                     "90x31+37x30+110x29+211x28+242x27+150x26+94x25+229x24+231x23+222x22+79x21+189x20+15x18+223x17+148x16+99x15+33x14+35x13+173x12+129x11+106x10+246x9+160x8+174x7+24x6+252x5+83x4+244x3+243x2+107x+80"))
    )
    )
    
  ))

(run-tests test-field-math)
