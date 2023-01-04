#lang racket

(require rackunit/text-ui)

(require "../../lib/share/gf.rkt")

(require rackunit "../../lib/decode/decode.rkt")

(define test-decode
  (test-suite
   "test-decode"
   
   (test-case
    "test-decode"
    
    ;; no errors
    (check-equal? (rs-decode 
                   '(32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17
                        196  35  39  119  235  215  231  226  93  23)
                   10)
                  '(32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17
                       196  35  39  119  235  215  231  226  93  23))

    ;; fix 1 error
    (check-equal? (rs-decode 
                   '(32 91 10 120 209 114 220 77 67 64 236 17 236 17 236 17
                        196  35  39  119  235  215  231  226  93  23)
                   10)
                  '(32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17
                       196  35  39  119  235  215  231  226  93  23))

    ;; fix 2 error
    (check-equal? (rs-decode 
                   '(32 91 10 120 209 114 220 77 67 64 236 17 236 17 235 17
                        196  35  39  119  235  215  231  226  93  23)
                   10)
                  '(32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17
                       196  35  39  119  235  215  231  226  93  23))

    ;; fix 3 error
    (check-equal? (rs-decode 
                   '(33 91 10 120 209 114 220 77 67 64 236 17 236 17 235 17
                        196  35  39  119  235  215  231  226  93  23)
                   10)
                  '(32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17
                       196  35  39  119  235  215  231  226  93  23))

    ;; fix 4 error
    (check-equal? (rs-decode 
                   '(33 91 10 120 209 114 220 77 67 64 236 17 236 17 235 17
                        196  35  39  119  235  215  231  226  93  24)
                   10)
                  '(32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17
                       196  35  39  119  235  215  231  226  93  23))

    ;; fix 5 error
    (check-equal? (rs-decode 
                   '(33 91 10 120 209 114 220 77 67 64 236 17 236 17 235 17
                        196  35  39  120  235  215  231  226  93  24)
                   10)
                  '(32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17
                       196  35  39  119  235  215  231  226  93  23))
    )


   (test-case
    "test-decode-4"

    ;; fix two errors
    (check-equal? (rs-decode 
                   '(1 2 3 4 5 11 7 8 9 10 11 3 1 12 12) 4
                   #:bit_width 4 #:primitive_poly_value 19)
                  '(1 2 3 4 5 6 7 8 9 10 11 3 3 12 12))

    ;; fix one errors
    (check-equal? (rs-decode 
                   '(1 2 3 4 5 6 7 8 9 10 11 3 1 12 12) 4
                   #:bit_width 4 #:primitive_poly_value 19)
                  '(1 2 3 4 5 6 7 8 9 10 11 3 3 12 12))

    ;; no error
    (check-equal? (rs-decode 
                   '(1 2 3 4 5 6 7 8 9 10 11 3 3 12 12) 4
                   #:bit_width 4 #:primitive_poly_value 19)
                  '(1 2 3 4 5 6 7 8 9 10 11 3 3 12 12))

    ;; three errors
    (check-equal? (rs-decode 
                   '(5 2 3 4 5 11 7 8 9 10 11 3 1 12 12) 4
                   #:bit_width 4 #:primitive_poly_value 19)
                   '(5 2 3 4 5 11 7 8 9 10 11 3 1 12 12))

    ;; all is errors
    (check-equal? (rs-decode 
                   '(12 12 1 3 11 10 9 8 7 6 5 4 3 2 1) 4
                   #:bit_width 4 #:primitive_poly_value 19)
                   '(12 12 1 3 11 10 9 8 7 6 5 4 3 2 1))

    ;; correction length is odd
    (check-equal? (rs-decode
                   '(1 2 3 4 5 11 7 8 9 0 11 15 11 11 0 15) 5
                   #:bit_width 4 #:primitive_poly_value 19)
                   '(1 2 3 4 5 6 7 8 9 10 11 15 11 11 0 15))

    )
   
   (test-case
    "test-decode-8"

    (check-equal?
     (rs-decode
      '(32 1 2 3 4 5 220 77 67 64 236 17 236 17 236 17 196 35 39 119 235 215 231 226 93 23) 10)
      '(32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17 196 35 39 119 235 215 231 226 93 23))

    ;; 5 errors recovery
    (check-equal?
     (list->bytes
      (take
       (rs-decode `(,@(bytes->list #"Chen Xiao is just a progr54321.")
                    ,@(bytes->list #"\372\275m\251\275\265LH^\255"))
                  10)
       31))
     (string->bytes/utf-8 "Chen Xiao is just a programmer."))

    ;; 6 errors can't recover
    (check-not-equal?
     (list->bytes
      (take
       (rs-decode `(,@(bytes->list #"Chen Xiao is just a prog654321.")
                    ,@(bytes->list #"\372\275m\251\275\265LH^\255"))
                  10)
       31))
     (string->bytes/utf-8 "Chen Xiao is just a programmer.")
     )

    ;; 17 errors, max recovery
    (check-equal?
     (list->bytes
      (take
       (rs-decode `(,@(bytes->list #"Chen Xiao is a fabulous artist.")
                    ,@(bytes->list #"\311\350\375\363Z\371\212\346o!IA\350\362\210\265\256\270\277\237\347\36 \233L\26\201\35\314.\310.e."))
                  34)
       31))
     (string->bytes/utf-8 "Chen Xiao is just a programmer."))

    ;; 18 errors, can't recover
    (check-equal?
     (list->bytes
      (take
       (rs-decode `(,@(bytes->list #"Chen Xiao is a fabulous artist!")
                    ,@(bytes->list #"\311\350\375\363Z\371\212\346o!IA\350\362\210\265\256\270\277\237\347\36 \233L\26\201\35\314.\310.e."))
                  34)
       31))
     (string->bytes/utf-8 "Chen Xiao is a fabulous artist!"))
    )

   ))

(run-tests test-decode)
