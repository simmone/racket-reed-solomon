#lang racket

(require rackunit/text-ui rackunit)

(require "../../src/field-math.rkt")
(require "../../src/decode.rkt")

(define test-decode
  (test-suite
   "test-decode"

   (test-case
    "GF256-fix-5-errors"

    (check-equal? (rs-decode 
                   '(32 91 10 121 209 114 220 77 67 64 236 16 235 17 236 17 196 35 39 119 235 215 231 226 93 22)
                   10)
                  '(32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17 196 35 39 119 235 215 231 226 93 23)))
   (test-case
    "GF256-decode-no-errors"

    (check-equal? (rs-decode 
                   '(32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17 196 35 39 119 235 215 231 226 93 23)
                   10)
                  '(32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17 196 35 39 119 235 215 231 226 93 23)))

   (test-case
    "GF256-fix-1-error"

    (check-equal? (rs-decode 
                   '(32 91 10 120 209 114 220 77 67 64 236 17 236 17 236 17 196 35 39 119 235 215 231 226 93 23)
                   10)
                  '(32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17 196 35 39 119 235 215 231 226 93 23)))

   (test-case
    "GF256-fix-2-errors"

    (check-equal? (rs-decode 
                   '(32 91 10 120 208 114 220 77 67 64 236 17 236 17 236 17 196 35 39 119 235 215 231 226 93 23)
                   10)
                  '(32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17 196 35 39 119 235 215 231 226 93 23)))

   (test-case
    "GF256-fix-3-errors"

    (check-equal? (rs-decode 
                   '(32 91 10 120 209 115 220 77 67 64 236 17 236 17 235 17 196 35 39 119 235 215 231 226 93 23)
                   10)
                  '(32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17 196 35 39 119 235 215 231 226 93 23)))

   (test-case
    "GF256-fix-4-errors"

    (check-equal? (rs-decode 
                   '(32 91 10 121 209 114 220 77 67 64 236 16 235 17 236 17 196 35 39 119 235 215 231 226 93 23)
                   10)
                  '(32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17 196 35 39 119 235 215 231 226 93 23)))

   (test-case
    "GF256 can't fix 6 errors"

    (check-not-equal? (rs-decode 
                       '(32 91 10 121 209 114 220 77 67 64 236 16 235 17 236 17 196 35 39 119 235 215 231 225 93 22)
                       10)
                      '(32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17 196 35 39 119 235 215 231 226 93 23))
    )

   (test-case
    "GF16 fix two errors"
    (check-equal? (rs-decode 
                   '(1 2 3 4 5 11 7 8 9 10 11 3 1 12 12) 4
                   #:bit_width 4 #:primitive_poly_value 19)
                  '(1 2 3 4 5 6 7 8 9 10 11 3 3 12 12)))

   (test-case
    "GF16 fix one errors"
    (check-equal? (rs-decode 
                   '(1 2 3 4 5 6 7 8 9 10 11 3 1 12 12) 4
                   #:bit_width 4 #:primitive_poly_value 19)
                  '(1 2 3 4 5 6 7 8 9 10 11 3 3 12 12)))

   (test-case
    "GF16 no error"
    (check-equal? (rs-decode 
                   '(1 2 3 4 5 6 7 8 9 10 11 3 3 12 12) 4
                   #:bit_width 4 #:primitive_poly_value 19)
                  '(1 2 3 4 5 6 7 8 9 10 11 3 3 12 12)))

   (test-case
    "GF16 three errors"
    (check-equal? (rs-decode 
                   '(5 2 3 4 5 11 7 8 9 10 11 3 1 12 12) 4
                   #:bit_width 4 #:primitive_poly_value 19)
                  '(5 2 3 4 5 11 7 8 9 10 11 3 1 12 12)))
    
   (test-case
    "GF16 all is errors"
    (check-equal? (rs-decode 
                   '(12 12 1 3 11 10 9 8 7 6 5 4 3 2 1) 4
                   #:bit_width 4 #:primitive_poly_value 19)
                  '(12 12 1 3 11 10 9 0 7 12 5 4 3 2 1)))

   (test-case
    "GF16 correction length is odd"
    (check-equal? (rs-decode
                   '(1 2 3 4 5 11 7 8 9 0 11 15 11 11 0 15) 5
                   #:bit_width 4 #:primitive_poly_value 19)
                  '(1 2 3 4 5 6 7 8 9 10 11 15 11 11 0 15)))
   
   (test-case
    "GF256 decode"
    (check-equal?
     (rs-decode
      '(32 1 2 3 4 5 220 77 67 64 236 17 236 17 236 17 196 35 39 119 235 215 231 226 93 23) 10)
     '(32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17 196 35 39 119 235 215 231 226 93 23)))

   (test-case
    "GF256 5 errors recovery"
    (check-equal?
     (list->bytes
      (take
       (rs-decode `(,@(bytes->list #"Chen Xiao is just a progr54321.")
                    ,@(bytes->list #"\372\275m\251\275\265LH^\255"))
                  10)
       31))
     (string->bytes/utf-8 "Chen Xiao is just a programmer.")))

   (test-case
    "GF256 6 errors can't recover"
    (check-not-equal?
     (list->bytes
      (take
       (rs-decode `(,@(bytes->list #"Chen Xiao is just a prog654321.")
                    ,@(bytes->list #"\372\275m\251\275\265LH^\255"))
                  10)
       31))
     (string->bytes/utf-8 "Chen Xiao is just a programmer.")
     ))

   (test-case
    "GF256 17 errors, max recovery"
    (check-equal?
     (list->bytes
      (take
       (rs-decode `(,@(bytes->list #"Chen Xiao is a fabulous artist.")
                    ,@(bytes->list #"\311\350\375\363Z\371\212\346o!IA\350\362\210\265\256\270\277\237\347\36 \233L\26\201\35\314.\310.e."))
                  34)
       31))
     (string->bytes/utf-8 "Chen Xiao is just a programmer.")))

   (test-case
    "GF256 18 errors, can't recover"
    (check-equal?
     (list->bytes
      (take
       (rs-decode `(,@(bytes->list #"Chen Xiao is a fabulous artist!")
                    ,@(bytes->list #"\311\350\375\363Z\371\212\346o!IA\350\362\210\265\256\270\277\237\347\36 \233L\26\201\35\314.\310.e."))
                  34)
       31))
     (string->bytes/utf-8 "Chen Xiao is a fabulous artist!"))
    )

   (test-case
    "GF256 can't recover, no exception"
    
    (rs-decode
     '(84 66 224 51 215 143 131 206 68 11 6 33 141 206 90 183 182 230 199 131 249 100 104 181 77 221 95 232 220 201 184 126 222 183 25 133 194 127 121 75 63 194 52 18 235 235 37 252 163 89 218 17 3 178 133 117 170 192 44 137 243 237 219 235 63 184 45 73 154 77 190 78 17 106 143 90 34 44 131 242 144 115 110 30 150 109 134 129 13 199 65 37 141 242 253 25 62 112 128 110 167 81 213 139 146 16 64 207 87 143 142 140 9 233 98 211 242 100 229 223 214 253 217 209 230 165 231 163 41 120 51 33 254 129 236 33 99 111 249 180 33 98 168 44 108 194 0 1 55 189 162 68 46 90 9 41 146 7 180 149 0 58 146 167 186 225 36 68 80 172 80 67 138 8 79 68 142 117 235)
     16)
    )

   ))

(run-tests test-decode)
