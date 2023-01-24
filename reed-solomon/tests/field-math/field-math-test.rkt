#lang racket

(require rackunit/text-ui rackunit)

(require "../../src/field-math.rkt")

(define test-field-math
  (test-suite 
   "test-field-math"
   
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
    "poly->index_coe_pairs && index_coe_pairs->poly"
    
    (check-equal? (poly->index_coe_pairs "1") '((0 . 1)))
    (check-equal? (index_coe_pairs->poly '((0 . 1))) "1")

    (check-equal? (poly->index_coe_pairs "4") '((0 . 4)))
    (check-equal? (index_coe_pairs->poly '((0 . 4))) "4")

    (check-equal? (poly->index_coe_pairs "x") '((1 . 1)))
    (check-equal? (poly->index_coe_pairs "x1") '((1 . 1)))
    (check-equal? (index_coe_pairs->poly '((1 . 1))) "x")

    (check-equal? (poly->index_coe_pairs "2x") '((1 . 2)))
    (check-equal? (index_coe_pairs->poly '((1 . 2))) "2x")

    (check-equal? (poly->index_coe_pairs "x4+x3+x2+x+1") '((4 . 1) (3 . 1) (2 . 1) (1 . 1) (0 . 1)))
    (check-equal? (poly->index_coe_pairs " x4 + x3+x2+x1+1 ") '((4 . 1) (3 . 1) (2 . 1) (1 . 1) (0 . 1)))
    (check-equal? (poly->index_coe_pairs "x4+x3+x1+x2+1") '((4 . 1) (3 . 1) (2 . 1) (1 . 1) (0 . 1)))
    (check-equal? (index_coe_pairs->poly '((4 . 1) (3 . 1) (2 . 1) (1 . 1) (0 . 1))) "x4+x3+x2+x+1")

    (check-equal? (index_coe_pairs->poly '((4 . 1) (3 . 1) (1 . 1) (2 . 1) (0 . 1))) "x4+x3+x2+x+1")
    )
   
   (test-case
    "galios-multiply"
    
    (parameterize*
     ([*field_generator_poly* "x4+x+1"])

     (check-equal? (galios-multiply 10 13) 11)
     (check-equal? (galios-multiply 14 14) 11)
     (check-equal? (galios-multiply 14 15) 5)
     (check-equal? (galios-multiply 3 6) 10)
     (check-equal? (galios-multiply 1 2) 2)
     (check-equal? (galios-multiply 0 0) 0)
     (check-equal? (galios-multiply 0 15) 0)
     (check-equal? (galios-multiply 15 0) 0)
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
     ))

   (test-case
    "get-generator-poly"
    
    (parameterize*
     ([*field_generator_poly* "x4+x+1"])
     (check-equal? (get-generator-poly 4) "x4+15x3+3x2+x+12"))

    (parameterize*
     ([*field_generator_poly* "x8+x4+x3+x2+1"])
     (check-equal? (get-generator-poly 8) "x16+59x15+13x14+104x13+189x12+68x11+209x10+30x9+8x8+163x7+65x6+41x5+229x4+98x3+50x2+36x+59"))
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

  ))

(run-tests test-field-math)
