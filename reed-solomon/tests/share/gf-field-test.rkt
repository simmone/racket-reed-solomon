#lang racket

(require rackunit/text-ui)

(require rackunit "../../lib/share/gf.rkt")

(define test-gf
  (test-suite 
   "test-gf"

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
    "test-gf-a->n"

    (parameterize*
     ([*bit_width* 8]
      [*2^m_1* (sub1 (expt 2 (*bit_width*)))]
      [*primitive_poly_value* 285]
      [*gf_aton_map* (get-gf-aton-hash)]
      [*gf_ntoa_map* (make-hash (hash-map (*gf_aton_map*) (lambda (a n) (cons n a))))])
    
     (check-equal? (poly-gf-a->n "a0x1+a0x0") "1x1+1x0")
     
     (check-equal? (poly-gf-a->n "x1+a3x0") "1x1+8x0")

     (check-equal? (poly-gf-a->n "a25x1+a8x0") "3x1+29x0")

     (check-equal? (poly-gf-a->n "a0x2+a0x1+a1x1+a1x0") "1x2+2x1+1x1+2x0")

     (check-equal? (poly-gf-a->n "a0x3+a25x2+a1x1+a2x2+a27x1+a3x0") "1x3+4x2+3x2+12x1+2x1+8x0")
    ))

   (test-case
    "test-gf-n->a"

    (parameterize*
     ([*bit_width* 8]
      [*2^m_1* (sub1 (expt 2 (*bit_width*)))]
      [*primitive_poly_value* 285]
      [*gf_aton_map* (get-gf-aton-hash)]
      [*gf_ntoa_map* (make-hash (hash-map (*gf_aton_map*) (lambda (a n) (cons n a))))])
    
     (check-equal? (poly-gf-n->a "a1x2+a3x1+a2x0") "a0x2+a25x1+a1x0")

     (check-equal? (poly-gf-n->a "a1x3+a7x2+a14x1+a8x0") "a0x3+a198x2+a199x1+a3x0")
     ))

   (test-case
    "test-gf-multiply"

    (parameterize*
     ([*bit_width* 8]
      [*2^m_1* (sub1 (expt 2 (*bit_width*)))])
    
     (check-equal? (poly-gf-multiply "a0x1+a0x0" "a0x1+a1x0") "a0x2+a1x1+a0x1+a1x0")

     (check-equal? (poly-gf-multiply "a0x1+a0x0" "x2") "a0x3+a0x2")

     (check-equal? (poly-gf-multiply "a1" "x2") "a1x2")

     (check-equal? (poly-gf-multiply "a" "x") "a1x1")

     (check-equal? (poly-gf-multiply "a3x4+a1x2" "a2x3+a0x1") "a5x7+a3x5+a3x5+a1x3")

     (check-equal? (poly-gf-multiply "a170x1" "a164x1") "a79x2")
     
     (check-equal? (poly-gf-multiply "x+a0" "x+a1") "a0x2+a1x1+a0x1+a1x0")
     )

    (parameterize*
     ([*bit_width* 4]
      [*2^m_1* (sub1 (expt 2 (*bit_width*)))]
      [*primitive_poly_value* 19]
      [*gf_aton_map* (get-gf-aton-hash)]
      [*gf_ntoa_map* (make-hash (hash-map (*gf_aton_map*) (lambda (a n) (cons n a))))])

     (check-equal? (poly-gf-a->n (poly-gf-multiply (poly-gf-n->a "12x3+4x2+3x+15") "a9x")) "1x4+14x3+13x2+12x1")

     (check-equal? (poly-gf-a->n (poly-gf-multiply (poly-gf-n->a "12x3+4x2+3x+15") "5x")) "1x4+14x3+13x2+12x1")
     ))

   ))

(run-tests test-gf)
