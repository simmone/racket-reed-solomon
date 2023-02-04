#lang racket

(require rackunit/text-ui rackunit)

(require "../../../src/field-math.rkt")
(require "../../../src/new_decode/error-locator.rkt")

(define test-error-locator
  (test-suite 
   "test-error-locator"

   (test-case
    "test-get-error-locator"

    (parameterize*
     ([*bit_width* 4]
      [*field_generator_poly* "x4+x+1"]
      [*galios_index->number_map* (get-galios-index->number_map (*bit_width*))]
      [*galios_number->index_map* (make-hash (hash-map (*galios_index->number_map*) (lambda (a n) (cons n a))))])

     (let-values ([(ome lam) (error-locator '(12 4  3 15) 2)])
       (check-equal? ome "6x+15")
       (check-equal? lam "14x2+14x+1")))
    )
   ))

(run-tests test-error-locator)
