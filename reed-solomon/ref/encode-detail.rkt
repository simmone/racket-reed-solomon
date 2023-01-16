#lang racket

(require "../src/encode/encode.rkt")
(require "../src/lib/gf.rkt")
(require "../src/lib/poly.rkt")
(require "../src/lib/euclidean.rkt")
(require "../src/lib/lib.rkt")

(let (
      [input_data_list
       '(197 115 41 125 254 185 37 168 67 214 59 104 236 180 183 46 125 136 152 13 175 0 201 226 255 174 166 36 220 218 35 32 195 102 169 3 107 51 78 69 84 153 38 161 4 126 45 119 204 233 84 22 89 26 214 173 46 50 213 199 125 171 53 5 242 117 76 241 92 230 138 132 249 43 46 196 116 253 222 129 166 200 121 147 34 10 234 28 147 240 122 28 86 247 9 55 132 31 137 154 189 118 217 226 176 104 205 128 138 234 166 150 87 141 165 230 237 167 206 228 76 68 131 105 225 198 207 217 14 127 81 48 32 182 112 1 117 181 22 142 195 197 223 158 208 56 153 142 52 28 232 40 198 68 122 194 73 214 24 143 202 57 105 159 106 100 225 254 19 139 107 82 43 239 69 206 184 92 190 189 215 5 135 122 234 38 113 61 221 170 19)]
      [parity_length 16]
      )

  (printf "start enode\n\n")

  (printf "input data list:\n\n")
  
  (display-list input_data_list)
  
  (printf "parity_length:~a\n\n" parity_length)

  (parameterize*
   ([*bit_width* 8]
    [*2^m_1* (sub1 (expt 2 (*bit_width*)))]
    [*primitive_poly_value* 285]
    [*gf_aton_map* (get-gf-aton-hash)]
    [*gf_ntoa_map* (make-hash (hash-map (*gf_aton_map*) (lambda (a n) (cons n a))))])
   
   (hash-set! (*gf_ntoa_map*) 0 256)

   (printf "*bit_width*:~a\n\n" (*bit_width*))

   (printf "*2^m-1*:~a\n\n" (*2^m_1*))
   
   (printf "*bit_width*:~a, primitive_poly:~a, *primitive_poly_value*=~a\n\n" (*bit_width*) "x^8+x^4+x^3+x^2+1" (*primitive_poly_value*))

   (printf "*gf_aton_map*:\n\n")

   (display-list (hash->list (*gf_aton_map*)) 15)

   (printf "*gf_ntoa_map*:\n\n")

   (display-list (hash->list (*gf_ntoa_map*)) 15)

   (let* ([generator_poly (generator-poly parity_length)]
          [message_poly (coeffients->poly-n input_data_list)])

     (printf "generator_poly = (generate-poly parity_length)):\n\n")

     (printf "~a\n\n" generator_poly)

     (printf "message_poly = (coeffients->poly-n input_list):\n\n")

     (printf "~a\n\n" message_poly)

     (printf "euclidean divide:\n\n")

     (let* ([parity_length_poly (format "x~a" parity_length)]
            [message_poly*parity_length
             (poly-gf-n-multiply message_poly parity_length_poly)]
            [generator_poly_n (poly-gf-a->n generator_poly)])
       
       (printf "parity_length_poly:~a\n\n" parity_length_poly)
       
       (printf "message_poly*parity_length = (poly-gf-n-multiply message_poly parity_length_poly)\n\n")

       (printf "~a\n\n" message_poly*parity_length)
       
       (printf "generator_poly_n = (poly-gf-a->n generator_poly):\n\n")
       
       (printf "~a\n\n" generator_poly_n)
       
       (printf "(euc-divide message_poly*parity_length generate_poly_n)\n\n")

       (let-values ([(quotient remainder) 
                     (euc-divide
                      message_poly*parity_length
                      generator_poly_n)])

         (printf "quotient:~a\n\n" quotient)

         (printf "remainder:~a\n\n" remainder)

         (let ([result (poly-n->coeffients remainder)])

           (printf "result: (poly-n->coeffients remainder)\n\n")

           (printf "result:~a\n\n" result)

           (when (< (length result) parity_length)
             (printf "result_length:~a < parity_length:~a\n\n" (length result) parity_length)
             
             (printf "should append 0: (set! result (append (make-list (- parity_length (length result)) 0) result))\n\n")

             (set! result (append (make-list (- parity_length (length result)) 0) result)))
           
           
           (printf "result data list:\n\n")
  
           (display-list result)
           
           (printf "encode end.\n\n")

           result))))))
