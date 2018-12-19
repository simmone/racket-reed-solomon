#lang racket

(require rackunit/text-ui)

(require rackunit "../../lib/encode/encode.rkt")

(define test-encode
  (test-suite 
   "test-encode"

   (test-case
    "test-encode"

    (check-equal?
     (rs-encode '(32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17) 10)
     (list 196  35  39  119  235  215  231  226  93  23))

    (check-equal? 
    (rs-encode '(32 91 11 120 209 114 220 77 67 64 236 17 236) 13)
     (list 168  72  22  82  217  54  156  0  46  15  180  122  16))

    (check-equal?
    (rs-encode '(1 2 3 4 5 6 7 8 9 10 11) 4 #:bit_width 4 #:primitive_poly_value 19)
    (list 3 3 12 12))

    (check-equal?
    (rs-encode '(1 2 3 4 5 6 7 8 9 10 11) 5 #:bit_width 4 #:primitive_poly_value 19)
    (list 15 11 11 0 15)) 
    )
   
   (test-case
    "test-encode-chars"
    
    (check-equal?
     (list->bytes
      (rs-encode
       (bytes->list (string->bytes/utf-8 "chenxiao is a simple programmer."))
       22
       #:express? #t))
     #"\337\2oH\16\364\226b\215\263\261\324p\312s8\312\327\350\334gZ")
    )
   
   ))

(run-tests test-encode)
