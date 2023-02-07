#lang racket

(require rackunit/text-ui rackunit)

(require "../../../src/field-math.rkt")
(require "../../../src/new_decode/syndrome.rkt")

(define test-syndrome
  (test-suite 
   "test-syndrome"

   (test-case
    "test-get-syndromes-gf16"

    (parameterize*
     ([*bit_width* 4]
      [*field_generator_poly* "x4+x+1"]
      [*galios_index->number_map* (get-galios-index->number_map (*bit_width*))])

     (check-equal? (get-syndromes 
                    '(1 2 3 4 5 11 7 8 9 10 11 3 1 12 12) 4)
                   '(12 4 3 15))

     (check-equal? (get-syndromes
                    '(12 12 1 3 11 10 9 8 7 6 5 4 3 2 1) 4)
                   '(7 14 2))
     ))

   (test-case
    "test-get-syndromes-gf256"

    (parameterize*
     ([*bit_width* 8]
      [*field_generator_poly* "x8+x4+x3+x2+1"]
      [*galios_index->number_map* (get-galios-index->number_map (*bit_width*))])

     (check-equal? (get-syndromes
                    '(32 91 10 121 209 114 220 77 67 64 236 16 235 17 236 17 196 35 39 119 235 215 231 226 93 22)
                    10)
                   '(127 213 228 134 89 149 113 122 131 7))

     (check-equal? (get-syndromes
                    '(248 146 101 20 154 230 111 233 94 213 1 93 180 149 155 81 253 215 246 143 121 234 121 19 172 146 19 15 170 25 3 93 89 58 63 51 156 203 103 230 157 102 132 246 74 75 14 50 50 125 148 194 1 144 15 98 36 222 214 1 242 232 68 48 254 100 102 143 142 194 199 92 140 18 93 43 230 28 206 110 194 76 135 0 21 105 163 172 251 99 243 175 68 158 186 81 17 106 173 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
                    16)
                   '(238 78 236 177 145 43 66 173 243 171 61 129 94 102 22 92))
     ))

    ))

(run-tests test-syndrome)
