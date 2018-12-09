#lang racket

(require rackunit/text-ui)

(require rackunit "../../lib/decode/syndrome.rkt")

(define (get-gf-aton-hash)
  (let ([aton_map (make-hash)])
    (let loop ([a 0]
               [last_n (/ 1 2)])
      (when (< a 15)
            (let ([n (* last_n 2)])
              (when (> n 15)
                    (set! n (bitwise-xor n 19)))

              (hash-set! aton_map a n)

              (loop (add1 a) n))))
    aton_map))

(define test-syndrome
  (test-suite 
   "test-syndrome"

   (test-case
    "test-get-syndromes"

    (check-equal? (get-syndromes 
                   '(1 2 3 4 5 11 7 8 9 10 11 3 1 12 12)
                   4)
                  '(12 4 3 15)))
    ))

(run-tests test-syndrome)
