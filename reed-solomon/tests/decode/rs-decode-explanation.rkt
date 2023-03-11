#lang racket

(require "../../src/field-math.rkt")
(require "../../src/decode/syndrome.rkt")
(require "../../src/decode/error-locator.rkt")
(require "../../src/decode/chien-search.rkt")
(require "../../src/decode/forney.rkt")
(require "../../src/primitive_poly_table.rkt")

(require "../lib.rkt")

(require rackunit)

(define (_rs-decode 
         data_list
         parity_length
         #:bit_width [bit_width 8]
         #:primitive_poly_value [primitive_poly_value 285])

  (parameterize*
   ([*bit_width* bit_width]
    [*field_generator_poly* (hash-ref *primitive_poly_table* primitive_poly_value)]
    [*galios_index->number_map* (get-galios-index->number_map)]
    [*galios_number->index_map* (make-hash (hash-map (*galios_index->number_map*) (lambda (a n) (cons n a))))])


   (let* ([*t* (floor (/ parity_length 2))]
          [*2^m_1* (sub1 (expt 2 (*bit_width*)))]
          [appended_data_list 
           (if (> *2^m_1* (length data_list))
               (append
                data_list
                (make-list (add1 (- *2^m_1* (length data_list))) 0))
               data_list)])

     (printf "Reed Solomon Decoding Explanation:\n\n")
     
     (printf "appended data list[~a]:\n\n" (length appended_data_list))
     
     (print-line appended_data_list)

     (printf "parity_length: ~a\n\n" parity_length)
     
     (printf "bit_width: ~a\n\n" (*bit_width*))

     (printf "primitive_poly_value: ~a\n\n" primitive_poly_value)

     (define syndromes (get-syndromes appended_data_list (* 2 *t*)))
     
     (printf "syndromes: (get-syndromes appended_data_list (* 2 *t*)) = ~a\n\n" syndromes)

     (if (null? syndromes)
         data_list
         (let-values ([(ome_poly lam_poly) (error-locator syndromes *t*)])
           (printf "(error-locator ~a ~a)\n" syndromes *t*)

           (printf "error-locater's ome_poly = ~a\n\n" ome_poly)
           (printf "error-locater's lam_poly = ~a\n\n" lam_poly)

           (if (not lam_poly)
               data_list
               (let ([err_places (chien-search lam_poly)]
                     [restored_list #f])
                 (printf "err_places = (chien-search ~a) = ~a\n\n" lam_poly err_places)
                 (set! restored_list
                       (if (null? err_places)
                           data_list
                           (let ([err_correct_pairs #f])

                             (printf "err_correct_pairs: (forney ~a ~a ~a) = " lam_poly ome_poly err_places)
                             (set! err_correct_pairs (forney lam_poly ome_poly err_places))
                             (printf "~a\n" err_correct_pairs)

                             (let loop ([patches err_correct_pairs]
                                        [loop_data_list (reverse appended_data_list)])
                               (if (not (null? patches))
                                   (let ([restored_data (bitwise-xor (cdar patches) (list-ref loop_data_list (caar patches)))])
                                     (printf "bitwise-xor ~a's ~a with ~a = ~a\n"
                                             (caar patches)
                                             (list-ref loop_data_list (caar patches))
                                             (cdar patches)
                                             restored_data)
                                     
                                     (loop
                                      (cdr patches)
                                      (list-set loop_data_list (caar patches) restored_data)))
                                   (reverse loop_data_list))))))
                 (printf "restored_list: ~a\n" (take restored_list (length data_list)))
                 (take restored_list (length data_list)))))))))

;(check-equal? (_rs-decode 
;               '(1 2 3 4 5 11 7 8 9 10 11 3 1 12 12) 4
;               #:bit_width 4 #:primitive_poly_value 19)
;              '(1 2 3 4 5 6 7 8 9 10 11 3 3 12 12))
;
;(check-equal? (_rs-decode 
;               '(32 91 10 121 209 114 220 77 67 64 236 16 235 17 236 17 196 35 39 119 235 215 231 226 93 22)
;               10)
;              '(32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17 196 35 39 119 235 215 231 226 93 23))
;
;(check-equal? (_rs-decode 
;               '(5 2 3 4 5 11 7 8 9 10 11 3 1 12 12) 4
;               #:bit_width 4 #:primitive_poly_value 19)
;              '(5 2 3 4 5 11 7 8 9 10 11 3 1 12 12))

;(check-equal? (_rs-decode 
;               '(12 12 1 3 11 10 9 8 7 6 5 4 3 2 1) 4
;               #:bit_width 4 #:primitive_poly_value 19)
;              '(12 12 1 3 11 10 9 0 7 12 5 4 3 2 1))

;(_rs-decode
; '(120 149 167 14 154 108 108 139 188 178 104 149 134 121 251 95 104 251 75 217 245 43 242 164 66 185 240 113 157 195 1 133 67 107 6 144 21 208 65 108 196 99 253 145 62 229 214 234 108 13 152 234 244 203 9 85 50 199 238 37 197 88 22 106 220 175 250 134 45 19 135 144 171 241 208 40 231 42 253 242 94 173 236 222 193 131 248 255 166 136 28 125 151 31 161 121 26 28 85 119 11 52 176 140 253 70 189 252 228 99)
; 16)
