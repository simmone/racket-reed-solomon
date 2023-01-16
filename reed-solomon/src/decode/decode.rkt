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
   
;;   (hash-set! (*gf_ntoa_map*) 0 0)

   (let (
         [appended_data_list 
          (if (> (*2^m_1*) (length raw_list))
              (append
               raw_list
               (make-list (- (*2^m_1*) (length raw_list)) 0))
              raw_list)]
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

     (set! syndromes (get-syndromes appended_data_list (* 2 t)))

     (set! syndrome_poly (coeffients->poly-n syndromes))
     
     (if (= (foldr + 0 syndromes) 0)
         raw_list
         (with-handlers
          ([exn:fail?
            (lambda (v)
              raw_list)])
          (let-values ([(ome_poly lam_poly) (error-locator-poly syndrome_poly t)])
            (set! err_places (chien-search lam_poly))
            
            (set! err_correct_pairs (forney lam_poly ome_poly err_places))

            (set! corrected_values 
                  (correct-error 
                   appended_data_list
                   err_correct_pairs))

            (take corrected_values (length raw_list))))))))



