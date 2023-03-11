#lang racket

(require rackunit/text-ui rackunit)

(require "../../../src/field-math.rkt")
(require "../../../src/decode/forney.rkt")

(define test-forney
  (test-suite 
   "test-forney"

   (test-case
    "test-forney"

    (parameterize*
     ([*bit_width* 4]
      [*field_generator_poly* "x4+x+1"]
      [*galios_index->number_map* (get-galios-index->number_map)]
      [*galios_number->index_map* (make-hash (hash-map (*galios_index->number_map*) (lambda (a n) (cons n a))))])

     (check-equal? (forney "14x2+14x+1" "6x+15" '(9 2)) '( (9 . 13)  (2 . 2)))))

   (test-case
    "test-calculate-factor"

    (parameterize*
     ([*bit_width* 4]
      [*field_generator_poly* "x4+x+1"]
      [*galios_index->number_map* (get-galios-index->number_map)]
      [*galios_number->index_map* (make-hash (hash-map (*galios_index->number_map*) (lambda (a n) (cons n a))))])

     (check-equal? (calculate-factor "10x+2" 6) 4)

     (check-equal? (calculate-factor "10x+2" 13) 14)

     (check-equal? (calculate-factor "6x+15" 6) 0)

     (check-equal? (calculate-factor "14x0" 6) 11)
     ))

   ))

(run-tests test-forney)
