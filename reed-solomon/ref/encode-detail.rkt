#lang racket

(require "../src/encode/encode.rkt")
(require "../src/lib/gf.rkt")
(require "../src/lib/poly.rkt")
(require "../src/lib/euclidean.rkt")
(require "../src/lib/lib.rkt")

(let (
      [input_data_list
       '(248 146 101 193 154 230 111 233 94 213 1 126 180 149 155 81 253 215 246 143 121 234 121 19 172 146 19 15 170 230 3 93 89 58 63 51 156 214 103 230 55 102 132 246 74 75 14 50 50 125 158 194 1 144 15 98 36 222 214 1 242 232 68 48 254 100 102 143 142 194 199 139 140 18 93 43 230 28 206 81 194 76 135)]
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
