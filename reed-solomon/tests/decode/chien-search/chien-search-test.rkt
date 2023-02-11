#lang racket

(require rackunit/text-ui rackunit)

(require "../../../src/decode/chien-search.rkt")
(require "../../../src/field-math.rkt")

(define test-chien-search
  (test-suite 
   "test-chien-search"

   (test-case
    "test-chien-value"

    (parameterize*
     ([*bit_width* 4]
      [*field_generator_poly* "x4+x+1"]
      [*galios_index->number_map* (get-galios-index->number_map (*bit_width*))]
      [*galios_number->index_map* (make-hash (hash-map (*galios_index->number_map*) (lambda (a n) (cons n a))))])
     
     (check-equal? (chien-value "14x2+14x1+1x0" 14) 3)
     (check-equal? (chien-value "14x2+14x1+1x0" 13) 13)
     (check-equal? (chien-value "14x2+14x1+1x0" 12) 12)
     (check-equal? (chien-value "14x2+14x1+1x0" 11) 3)
     (check-equal? (chien-value "14x2+14x1+1x0" 10) 15)
     (check-equal? (chien-value "14x2+14x1+1x0" 9) 0)
     (check-equal? (chien-value "14x2+14x1+1x0" 8) 14)
     (check-equal? (chien-value "14x2+14x1+1x0" 7) 13)
     (check-equal? (chien-value "14x2+14x1+1x0" 6) 14)
     (check-equal? (chien-value "14x2+14x1+1x0" 5) 15)
     (check-equal? (chien-value "14x2+14x1+1x0" 4) 2)
     (check-equal? (chien-value "14x2+14x1+1x0" 3) 2)
     (check-equal? (chien-value "14x2+14x1+1x0" 2) 0)
     (check-equal? (chien-value "14x2+14x1+1x0" 1) 12)
     (check-equal? (chien-value "14x2+14x1+1x0" 0) 1)
     
     (check-equal? (chien-search "14x2+14x1+1x0") '(9 2))
     ))

   (test-case
    "test-chien-value2"

    (parameterize*
     ([*bit_width* 8]
      [*field_generator_poly* "x8+x4+x3+x2+1"]
      [*galios_index->number_map* (get-galios-index->number_map (*bit_width*))]
      [*galios_number->index_map* (make-hash (hash-map (*galios_index->number_map*) (lambda (a n) (cons n a))))])

     (check-equal? (chien-search "148x8+38x7+153x6+74x5+43x4+7x3+226x2+102x1+1x0") '())
     ))
   ))

(run-tests test-chien-search)
