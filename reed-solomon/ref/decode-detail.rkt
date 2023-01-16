#lang racket

(require "../src/decode/decode.rkt")
(require "../src/decode/syndrome.rkt")
(require "../src/decode/error-locator-poly.rkt")
(require "../src/decode/chien-search.rkt")
(require "../src/decode/forney.rkt")
(require "../src/decode/correct-error.rkt")
(require "../src/lib/gf.rkt")
(require "../src/lib/poly.rkt")
(require "../src/lib/euclidean.rkt")
(require "../src/lib/lib.rkt")

(let (
      [input_data_list '(215 134 230 162 94 212 85 175 164 168 225 56 61 100 23 221 109 73 32 182 229 196 140 163 7 32 39 26 73 230 97 238 2 77 40 113 20 167 59 234 53 230 188 68 64 149 106 202 226 229 60 105 12 93 227 107 252 168 128 98 87 138 134 177 12 136 93 227 151 253 27 174 125 119 164 58 42 59 196 236 176 96 231 179 1 16 194 100 54 182 112 89 78 241 241 120 38 109 227 131 9 32 64 0 222 160 75 25 86 253 154 132 251 165 219 90 222 151 1)]
      [parity_length 16]
      )

  (printf "start decode\n\n")

  (printf "input data list:\n\n")
  
  (display-list input_data_list)
  
  (printf "parity_length:~a\n\n" parity_length)

  (parameterize*
   ([*bit_width* 8]
    [*2^m_1* (sub1 (expt 2 (*bit_width*)))]
    [*primitive_poly_value* 285]
    [*gf_aton_map* (get-gf-aton-hash)]
    [*gf_ntoa_map* (make-hash (hash-map (*gf_aton_map*) (lambda (a n) (cons n a))))])

   (hash-set! (*gf_ntoa_map*) 0 0)

   (printf "*bit_width*:~a\n\n" (*bit_width*))

   (printf "*2^m-1*:~a\n\n" (*2^m_1*))
   
   (printf "*bit_width*:~a, primitive_poly:~a, *primitive_poly_value*=~a\n\n" (*bit_width*) "x^8+x^4+x^3+x^2+1" (*primitive_poly_value*))

   (printf "*gf_aton_map*:\n\n")

   (display-list (hash->list (*gf_aton_map*)) 15)

   (printf "*gf_ntoa_map*:\n\n")

   (display-list (hash->list (*gf_ntoa_map*)) 15)

   (let (
         [t #f]
         [syndromes #f]
         [syndrome_poly #f]
         [lam_derivative_poly #f]
         [Yj_poly #f]
         [err_places #f]
         [err_correct_pairs #f]
         [corrected_values #f]
         )
     
     (set! t (floor (/ parity_length 2)))
     
     (printf "t: ~a\n\n" t)

     (set! syndromes (get-syndromes input_data_list (* 2 t)))
     
     (printf "syndromes: [(get-syndromes input_data_list (* 2 t))]: [~a]\n\n" syndromes)

     (set! syndrome_poly (coeffients->poly-n syndromes))

     (printf "syndrome_poly: ~a\n\n" syndrome_poly)

     (define decoded_result
       (if (= (foldr + 0 syndromes) 0)
           (begin
             (printf "syndromes sum == 0, return input data list")
             input_data_list)
           (with-handlers
            ([exn:fail?
              (lambda (v)
                (printf "exception happens: ~a\n\n" v)
                input_data_list)])
            (let-values ([(ome_poly lam_poly) (error-locator-poly syndrome_poly t)])
              (printf "error-locator-poly result:\n")
              (printf "ome_poly: ~a\n\n" ome_poly)
              (printf "lam_poly: ~a\n\n" lam_poly)

              (set! err_places (chien-search lam_poly))
              (printf "error places:\n\n")
              (display-list err_places)
              (printf "error places convert to actual index, index from 1:\n\n")
              (display-list (map (lambda (err) (- 255 err)) err_places))
              
              (set! err_correct_pairs (forney lam_poly ome_poly err_places))
              (printf "error correct pairs:\n\n")
              (display-list err_correct_pairs)

              (set! corrected_values 
                    (correct-error 
                     input_data_list
                     err_correct_pairs))
              (printf "corrected values:\n\n")
              (display-list corrected_values)

              corrected_values))))
     
     (printf "decoded result:\n\n")

     (display-list decoded_result)
     
     (printf "take actual data:\n\n")
     
     (display-list (take decoded_result (length input_data_list)))
     
     (printf "decode end.\n\n")
   )))
