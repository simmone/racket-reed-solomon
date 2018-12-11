#lang racket

(require rackunit/text-ui)

(require rackunit "../../encode.rkt")

(define test-encode
  (test-suite 
   "test-encode"

   (test-case
    "test-encode"

    (check-equal? 
     (encode '(32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17) 10)
     (list 196  35  39  119  235  215  231  226  93  23))

    (check-equal? 
    (encode '(32 91 11 120 209 114 220 77 67 64 236 17 236) 13)
     (list 168  72  22  82  217  54  156  0  46  15  180  122  16))

    (check-equal? 
    (encode '(1 2 3 4 5 6 7 8 9 10 11) 4 #:bit_width 4 #:primitive_poly_value 19 #:express? #t)
    (list 3 3 12 12))
    )
   
   ))

(run-tests test-encode)