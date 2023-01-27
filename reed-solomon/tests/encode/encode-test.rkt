#lang racket

(require rackunit/text-ui rackunit)

(require "../../src/encode.rkt")

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

    (check-equal?
     (rs-encode '(35  37  245 131 35  83  116 84  83) 16)
     (list 0 154 220 253 68  10  124 102 201 53  167 140 96  91  50  66))
    )
   
   (test-case
    "test-encode-chars"
    
    (check-equal?
     (list->bytes
      (rs-encode
       (bytes->list (string->bytes/utf-8 "Chen Xiao is just a programmer."))
       10))
     #"\372\275m\251\275\265LH^\255"
     )

    (check-equal?
     (list->bytes
      (rs-encode
       (bytes->list (string->bytes/utf-8 "Chen Xiao is just a programmer."))
       34))
     #"\311\350\375\363Z\371\212\346o!IA\350\362\210\265\256\270\277\237\347\36 \233L\26\201\35\314.\310.e."
     )
    )

   ))

(run-tests test-encode)
