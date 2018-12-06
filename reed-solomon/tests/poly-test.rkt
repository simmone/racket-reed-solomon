#lang racket

(require rackunit/text-ui)

(require rackunit "../lib/poly.rkt")

(define test-poly
  (test-suite 
   "test-poly"

   (test-case
    "test-get-gf-hash"

    (parameterize*
     ([*bit_width* 8]
      [*2^m_1* (sub1 (expt 2 (*bit_width*)))]
      [*primitive_poly_value* 285])

     (let ([aton_map (get-gf-aton-hash)])

       (check-equal? (hash-ref aton_map 0) 1)
       (check-equal? (hash-ref aton_map 1) 2)
       (check-equal? (hash-ref aton_map 2) 4)
       (check-equal? (hash-ref aton_map 7) 128)
       (check-equal? (hash-ref aton_map 8) 29)
       (check-equal? (hash-ref aton_map 9) 58)
       (check-equal? (hash-ref aton_map 69) 47)
       (check-equal? (hash-ref aton_map 79) 240)
       (check-equal? (hash-ref aton_map 204) 221)
       (check-equal? (hash-ref aton_map 254) 142)
       )))

   (test-case
    "test-get-gf-hash"

    (parameterize*
     ([*bit_width* 4]
      [*2^m_1* (sub1 (expt 2 (*bit_width*)))]
      [*primitive_poly_value* 19])
    
     (let* ([aton_map (get-gf-aton-hash)])
       (check-equal? (hash-ref aton_map 0) 1)
       (check-equal? (hash-ref aton_map 1) 2)
       (check-equal? (hash-ref aton_map 2) 4)
       (check-equal? (hash-ref aton_map 3) 8)
       (check-equal? (hash-ref aton_map 4) 3)
       (check-equal? (hash-ref aton_map 5) 6)
       (check-equal? (hash-ref aton_map 6) 12)
       (check-equal? (hash-ref aton_map 7) 11)
       (check-equal? (hash-ref aton_map 8) 5)
       (check-equal? (hash-ref aton_map 9) 10)
       (check-equal? (hash-ref aton_map 10) 7)
       (check-equal? (hash-ref aton_map 11) 14)
       (check-equal? (hash-ref aton_map 12) 15)
       (check-equal? (hash-ref aton_map 13) 13)
       (check-equal? (hash-ref aton_map 14) 9)
       )))
    
   (test-case
    "test-poly-a->n"

    (parameterize*
     ([*bit_width* 8]
      [*2^m_1* (sub1 (expt 2 (*bit_width*)))]
      [*primitive_poly_value* 285]
      [*gf_aton_map* (get-gf-aton-hash)]
      [*gf_ntoa_map* (make-hash (hash-map (*gf_aton_map*) (lambda (a n) (cons n a))))])
    
     (check-equal? (poly-a->n "a0x1+a0x0") "1x1+1x0")
     
     (check-equal? (poly-a->n "x1+a3x0") "1x1+8x0")

     (check-equal? (poly-a->n "a25x1+a8x0") "3x1+29x0")

     (check-equal? (poly-a->n "a0x2+a0x1+a1x1+a1x0") "1x2+2x1+1x1+2x0")

     (check-equal? (poly-a->n "a0x3+a25x2+a1x1+a2x2+a27x1+a3x0") "1x3+4x2+3x2+12x1+2x1+8x0")
    ))

   (test-case
    "test-poly-n->a"

    (parameterize*
     ([*bit_width* 8]
      [*2^m_1* (sub1 (expt 2 (*bit_width*)))]
      [*primitive_poly_value* 285]
      [*gf_aton_map* (get-gf-aton-hash)]
      [*gf_ntoa_map* (make-hash (hash-map (*gf_aton_map*) (lambda (a n) (cons n a))))])
    
     (check-equal? (poly-n->a "a1x2+a3x1+a2x0") "a0x2+a25x1+a1x0")

     (check-equal? (poly-n->a "a1x3+a7x2+a14x1+a8x0") "a0x3+a198x2+a199x1+a3x0")
     ))

   (test-case
    "test-poly-multiply"

    (parameterize*
     ([*bit_width* 8]
      [*2^m_1* (sub1 (expt 2 (*bit_width*)))])
    
     (check-equal? (poly-multiply "a0x1+a0x0" "a0x1+a1x0") "a0x2+a1x1+a0x1+a1x0")

     (check-equal? (poly-multiply "a0x1+a0x0" "x2") "a0x3+a0x2")

     (check-equal? (poly-multiply "a1" "x2") "a1x2")

     (check-equal? (poly-multiply "a" "x") "a1x1")

     (check-equal? (poly-multiply "a3x4+a1x2" "a2x3+a0x1") "a5x7+a3x5+a3x5+a1x3")

     (check-equal? (poly-multiply "a170x1" "a164x1") "a79x2")
     
     (check-equal? (poly-multiply "x+a0" "x+a1") "a0x2+a1x1+a0x1+a1x0")
    ))

   (test-case
    "test-string->poly"
    
    (check-equal? (string->poly "a0x1+a0x0") '( (0 . 1) (0 . 0) ))
    (check-equal? (string->poly "a0x1-a0x0") '( (0 . 1) (0 . 0) ))
    (check-equal? (string->poly "a1-x1") '( (1 . 0) (0 . 1) ))
    (check-equal? (string->poly "a1-a2") '( (1 . 0) (2 . 0) ))
    (check-equal? (string->poly "a1x7") '( (1 . 7) ))
    (check-equal? (string->poly "a") '( (1 . 0) ))
    (check-equal? (string->poly "x") '( (0 . 1) ))
    (check-equal? (string->poly "") '( (0 . 0) ))
    (check-equal? (string->poly "a2") '( (2 . 0) ))
    (check-equal? (string->poly "x2") '( (0 . 2) ))
    (check-equal? (string->poly "ax") '( (1 . 1) ))
    (check-equal? (string->poly "1x2") '( (1 . 2) ))
    (check-equal? (string->poly "1") '( (1 . 0) ))
    
    )
   
   (test-case
    "test-poly->string"

    (check-equal? (poly->string '( (0 . 1) (0 . 0) )) "a0x1+a0x0")
    (check-equal? (poly->string '( (0 . 1) (0 . 0) )) "a0x1+a0x0")
    (check-equal? (poly->string '( (1 . 0) (0 . 1) )) "a0x1+a1x0")
    (check-equal? (poly->string '( (1 . 0) (2 . 0) )) "a2x0+a1x0")
    (check-equal? (poly->string '( (1 . 7) )) "a1x7")
    (check-equal? (poly->string '( (1 . 0) (3 . 4) (5 . 2) (2 . 3) (3 . 3) (2 . 0) )) "a3x4+a3x3+a2x3+a5x2+a2x0+a1x0")
    (check-equal? (poly->string '()) "")
    )

   (test-case
    "test-poly-combine-a"
    
    (check-equal? (poly-combine-a "3x1+2x1+8x0") "a1x1+a8x0")
    (check-equal? (poly-combine-a "3x2+2x1+0x1+8x0+5x0") "a3x2+a2x1+a13x0")

    (check-equal? (poly-combine-a "1x2+2x1+1x1+2x0") "a1x2+a3x1+a2x0")

    (check-equal? (poly-combine-a "1x3+4x2+3x2+12x1+2x1+8x0") "a1x3+a7x2+a14x1+a8x0")
    )

   (test-case
    "test-poly-cdr"
    
    (check-equal? (poly-cdr "x1+x5+x6") "a0x6+a0x5")
    (check-equal? (poly-cdr "x6") "")
    )
   
   (test-case
    "test-poly-car"
    
    (check-equal? (poly-car "x5+x4") "a0x5")
    )
   
   ))

(run-tests test-poly)
