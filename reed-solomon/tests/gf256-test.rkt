#lang racket

(require rackunit/text-ui)

(require rackunit "../lib/gf256.rkt")

(define test-gf256
  (test-suite 
   "test-gf256"

   (test-case
    "test-get-gf256-hash"
    
    (let* ([result (get-gf256-hash)]
           [aton_map (car result)]
           [ntoa_map (cdr result)])

    (check-equal? (hash-ref aton_map 0) 1)
    (check-equal? (hash-ref aton_map 1) 2)
    (check-equal? (hash-ref aton_map 2) 4)
    (check-equal? (hash-ref aton_map 7) 128)
    (check-equal? (hash-ref aton_map 8) 29)
    (check-equal? (hash-ref aton_map 69) 47)
    (check-equal? (hash-ref aton_map 204) 221)
    (check-equal? (hash-ref aton_map 254) 142)

    (check-equal? (hash-ref ntoa_map 1) 0)
    (check-equal? (hash-ref ntoa_map 2) 1)
    (check-equal? (hash-ref ntoa_map 4) 2)
    (check-equal? (hash-ref ntoa_map 129) 112)
    (check-equal? (hash-ref ntoa_map 29) 8)
    (check-equal? (hash-ref ntoa_map 47) 69)
    (check-equal? (hash-ref ntoa_map 221) 204)
    (check-equal? (hash-ref ntoa_map 142) 254)

    ))

   ))

(run-tests test-gf256)
