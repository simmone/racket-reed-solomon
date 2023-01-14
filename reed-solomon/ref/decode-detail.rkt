#lang racket

(require "../src/decode/decode.rkt")
(require "../src/decode/syndrome.rkt")
(require "../src/lib/gf.rkt")
(require "../src/lib/poly.rkt")
(require "../src/lib/euclidean.rkt")

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

;;    (check-equal? (rs-decode 
;;                   '(32 91 10 121 209 114 220 77 67 64 236 16 235 17 236 17 196 35 39 119 235 215 231 226 93 22)
;;                   10)
;;                   '(32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17 196 35 39 119 235 215 231 226 93 23)))

(let (
      [input_data_list '(32 91 10 121 209 114 220 77 67 64 236 16 235 17 236 17 196 35 39 119 235 215 231 226 93 22)]
      [parity_length 10]
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

     )

   (printf "decode end.\n\n")
   ))
