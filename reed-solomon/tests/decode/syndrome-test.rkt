#lang racket

(require rackunit/text-ui)

(require rackunit "../../lib/decode/syndrome.rkt")

(define test-syndrome
  (test-suite 
   "test-syndrome"

   (test-case
    "test-get-syndromes"

    (check-equal? (get-syndromes 
                   '(1 2 3 4 5 11 7 8 9 10 11 3 1 12 12)
                   4)
                  '(12 4 3 15))
    )
   
   ))

(run-tests test-syndrome)
