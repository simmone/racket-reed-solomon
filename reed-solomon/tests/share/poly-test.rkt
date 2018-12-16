#lang racket

(require rackunit/text-ui)

(require rackunit "../../lib/share/poly.rkt")

(define test-poly
  (test-suite 
   "test-poly"

   (test-case
    "test-string-a->poly"
    
    (check-equal? (string-a->poly "a0x1+a0x0") '( (0 . 1) (0 . 0) ))
    (check-equal? (string-a->poly "a0x1-a0x0") '( (0 . 1) (0 . 0) ))
    (check-equal? (string-a->poly "a1-x1") '( (1 . 0) (0 . 1) ))
    (check-equal? (string-a->poly "a1-a2") '( (1 . 0) (2 . 0) ))
    (check-equal? (string-a->poly "a1x7") '( (1 . 7) ))
    (check-equal? (string-a->poly "a") '( (1 . 0) ))
    (check-equal? (string-a->poly "x") '( (0 . 1) ))
    (check-equal? (string-a->poly "") '( (0 . 0) ))
    (check-equal? (string-a->poly "a2") '( (2 . 0) ))
    (check-equal? (string-a->poly "x2") '( (0 . 2) ))
    (check-equal? (string-a->poly "ax") '( (1 . 1) ))
    (check-equal? (string-a->poly "1x2") '( (1 . 2) ))
    (check-equal? (string-a->poly "1") '( (1 . 0) ))

    (check-equal? (string-a->poly "x4") '( (0 . 4) ))
    )
    
   (test-case
    "test-string-n->poly"
    
    (check-equal? (string-n->poly "x4") '( (1 . 4) ))

    (check-equal? (string-n->poly "0x4+14x3") '( (0 . 4) (14 . 3) ))
    )
   
   (test-case
    "test-poly-a->string"

    (check-equal? (poly-a->string '( (0 . 1) (0 . 0) )) "a0x1+a0x0")
    (check-equal? (poly-a->string '( (0 . 1) (0 . 0) )) "a0x1+a0x0")
    (check-equal? (poly-a->string '( (1 . 0) (0 . 1) )) "a0x1+a1x0")
    (check-equal? (poly-a->string '( (1 . 0) (2 . 0) )) "a2x0+a1x0")
    (check-equal? (poly-a->string '( (1 . 7) )) "a1x7")
    (check-equal? (poly-a->string '( (1 . 0) (3 . 4) (5 . 2) (2 . 3) (3 . 3) (2 . 0) )) "a3x4+a3x3+a2x3+a5x2+a2x0+a1x0")
    (check-equal? (poly-a->string '()) "")
    )

   (test-case
    "test-poly-n->string"

    (check-equal? (poly-n->string '( (1 . 1) (1 . 0) )) "1x1+1x0")
    )

   (test-case
    "test-poly-n-combine"
    
    (check-equal? (poly-n-combine "3x1+2x1+8x0") "1x1+8x0")

    (check-equal? (poly-n-combine "3x2+2x1+0x1+8x0+5x0") "3x2+2x1+13x0")

    (check-equal? (poly-n-combine "1x2+2x1+1x1+2x0") "1x2+3x1+2x0")

    (check-equal? (poly-n-combine "1x3+4x2+3x2+12x1+2x1+8x0") "1x3+7x2+14x1+8x0")

    (check-equal? (poly-n-combine "1x4+1x4+14x3+13x2+12x1") "0x4+14x3+13x2+12x1")
    )
   
   (test-case
    "test-poly-n-add"

    (check-equal? (poly-n-add "1x4" "1x4+14x3+13x2+12x1") "14x3+13x2+12x1")

    (check-equal? (poly-n-add "x4" "1x4+14x3+13x2+12x1") "14x3+13x2+12x1")
    )
   
   (test-case
    "test-poly-n->coeffients"

    (check-equal? (poly-n->coeffients
                  "72x10+69x9+76x8+76x7+79x6+32x5+87x4+79x3+82x2+76x1+68x0")
                  '(72 69 76 76 79 32 87 79 82 76 68))
    
    (check-equal? (poly-n->coeffients
                  "32x15+91x14+11x13+120x12+209x11+114x10+220x9+77x8+67x7+64x6+236x5+17x4+236x3+17x2+236x1+17x0")
                  '(32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17))
    )

   (test-case
    "test-coeffients->poly-n"

    (check-equal? (coeffients->poly-n '(72 69 76 76 79 32 87 79 82 76 68))
                  "72x10+69x9+76x8+76x7+79x6+32x5+87x4+79x3+82x2+76x1+68x0")
    
    (check-equal? (coeffients->poly-n '(32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17))
                  "32x15+91x14+11x13+120x12+209x11+114x10+220x9+77x8+67x7+64x6+236x5+17x4+236x3+17x2+236x1+17x0")
    )

   (test-case
    "test-poly-n-car"

    (check-equal? (poly-n-car "a1x4") '(1 . 4))
    
    (check-equal? (poly-n-car "1x4") '(1 . 4))

    (check-equal? (poly-n-car "1") '(1 . 0))

    (check-equal? (poly-n-car "") '(0 . 0))

    (check-equal? (poly-n-car "x") '(1 . 1))
    )


   (test-case
    "test-poly-n-tail"

    (check-equal? (poly-n-tail "a1x4") '(1 . 4))
    
    (check-equal? (poly-n-tail "2x5+1x4") '(1 . 4))

    (check-equal? (poly-n-tail "1") '(1 . 0))

    (check-equal? (poly-n-tail "") '(0 . 0))

    (check-equal? (poly-n-tail "x2+x+1") '(1 . 0))
    )

   ))

(run-tests test-poly)
