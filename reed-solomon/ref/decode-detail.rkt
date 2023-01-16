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
      [input_data_list '(197 115 41 125 254 185 37 168 67 214 59 104 236 180 183 46 125 136 152 13 175 0 201 226 255 133 166 36 220 218 35 32 195 102 169 3 107 51 78 69 84 153 38 161 4 126 45 119 204 233 84 22 89 26 214 173 46 50 213 199 125 171 53 5 242 117 76 241 92 230 138 123 249 43 46 196 116 253 222 129 166 200 121 147 34 10 234 28 147 240 122 28 86 247 9 55 132 31 137 154 189 118 217 226 176 176 205 128 138 234 166 150 87 141 165 230 36 167 206 228 76 68 46 105 225 198 207 217 14 127 81 48 32 182 112 1 117 181 22 142 195 197 223 158 208 56 153 142 52 28 232 40 48 68 122 194 73 214 24 143 202 57 105 159 106 100 225 254 19 139 107 82 43 239 69 206 92 92 190 189 215 5 135 122 234 38 113 61 221 170 19 43 202 67 226 87 108 88 139 146 216 5 121 74 4 211 158)]
      [parity_length 16]
      [appended_data_list #f]
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

   (set! appended_data_list
         (append
          input_data_list
          (make-list (- (*2^m_1*) (length input_data_list)) 0)))
  
   (printf "appened data list:\n\n")

   (display-list appended_data_list)
  
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

     (set! syndromes (get-syndromes appended_data_list (* 2 t)))
     
     (printf "syndromes: [(get-syndromes appended_data_list (* 2 t))]: [~a]\n\n" syndromes)

     (set! syndrome_poly (coeffients->poly-n syndromes))

     (printf "syndrome_poly: ~a\n\n" syndrome_poly)

     (define decoded_result
       (if (= (foldr + 0 syndromes) 0)
           (begin
             (printf "syndromes sum == 0, return input data list")
             appended_data_list)
           (with-handlers
            ([exn:fail?
              (lambda (v)
                (printf "exception happens: ~a\n\n" v)
                appended_data_list)])
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
                     appended_data_list
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
