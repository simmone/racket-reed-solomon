#lang racket

(require "../lib/gf.rkt")
(require "../lib/poly.rkt")
(require "../lib/euclidean.rkt")

(require "syndrome.rkt")
(require "error-locator-poly.rkt")
(require "chien-search.rkt")
(require "forney.rkt")
(require "correct-error.rkt")

(provide (contract-out
          [rs-decode (->* 
                      ((listof exact-integer?) natural?) 
                      (#:bit_width natural? #:primitive_poly_value natural?) 
                      (listof exact-integer?))]
          ))

(define (rs-decode 
         raw_list
         parity_length
         #:bit_width [bit_width 8]
         #:primitive_poly_value [primitive_poly_value 285])

  (parameterize*
   ([*bit_width* bit_width]
    [*2^m_1* (sub1 (expt 2 (*bit_width*)))]
    [*primitive_poly_value* primitive_poly_value]
    [*gf_aton_map* (get-gf-aton-hash)]
    [*gf_ntoa_map* (make-hash (hash-map (*gf_aton_map*) (lambda (a n) (cons n a))))])
   
   (hash-set! (*gf_ntoa_map*) 0 0)

   (printf "s0\n")

   (let (
         [appended_data_list 
          (if (> (*2^m_1*) (length raw_list))
              (append
               raw_list
               (make-list (- (*2^m_1*) (length raw_list)) 0))
              raw_list)]
         [t #f]
         [syndromes #f]
         [syndromes_sum #f]
         [syndrome_poly #f]
         [lam_derivative_poly #f]
         [Yj_poly #f]
         [err_places #f]
         [err_correct_pairs #f]
         [corrected_values #f]
         [result #f]
         )
     
     (set! t (floor (/ parity_length 2)))

     (printf "s1\n")

     (set! syndromes (get-syndromes appended_data_list (* 2 t)))

     (printf "s2\n")

     (set! syndrome_poly (coeffients->poly-n syndromes))

     (printf "s3, syndromes_poly: ~a\n" syndrome_poly)
     
     (set! syndromes_sum (foldr + 0 syndromes))
     
     (printf "syndromes sum:~a\n" syndromes_sum)
     
     (if (= syndromes_sum 0)
         (set! result raw_list)
         (let-values ([(ome_poly lam_poly) (error-locator-poly (poly-n-strip syndrome_poly) t)])
           (if (not lam_poly)
               (set! result raw_list)
               (begin
                 (printf "chinen-search: ~a\n" lam_poly)

                 (set! err_places (chien-search lam_poly))

                 (printf "s4\n")

                 (if (null? err_places)
                     (set! result raw_list)
                     (begin
                       (set! err_correct_pairs (forney lam_poly ome_poly err_places))

                       (printf "s5\n")

                       (set! corrected_values 
                             (correct-error 
                              appended_data_list
                              err_correct_pairs))

                       (printf "s6\n")

                       (set! result (take corrected_values (length raw_list)))))))))

     (printf "s7\n")
     
     result
     )))
