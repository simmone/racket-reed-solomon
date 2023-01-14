#lang racket

(require "../src/encode/encode.rkt")
(require "../src/lib/gf.rkt")
(require "../src/lib/poly.rkt")
(require "../src/lib/euclidean.rkt")
(require "../src/lib/generator-poly.rkt")

(define (display-list input_list [col_width 12] [line_count 10])
  (let loop ([loop_list input_list]
             [item_count 1])
    (when (not (null? loop_list))
      (let ([item (~a #:min-width col_width #:align 'left #:right-pad-string " " (format "[~a]" (car loop_list)))])
        (if (<= item_count line_count)
            (begin
              (printf "~a" item)
              
              (loop (cdr loop_list) (add1 item_count)))
            (begin
              (printf "\n~a" item)

              (loop (cdr loop_list) 2))))))
  (printf "\n\n"))

;;    (check-equal?
;;     (rs-encode '(32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17) 10)
;;     (list 196  35  39  119  235  215  231  226  93  23))

(let (
      [input_data_list '(32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17)]
      [parity_length 10]
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
