#lang racket

(require "lib/share/lib.rkt")
(require "lib/share/gf.rkt")
(require "lib/share/poly.rkt")
(require "lib/share/euclidean.rkt")

(require "lib/decode/syndrome.rkt")
(require "lib/decode/error-locator-poly.rkt")
(require "lib/decode/chien-search.rkt")
(require "lib/decode/forney.rkt")
(require "lib/decode/correct-error.rkt")

(provide (contract-out
          [decode (->* 
                   ((listof exact-integer?) natural?) 
                   (#:bit_width natural? #:primitive_poly_value natural? #:express? boolean? #:express_path path-string?) 
                   (listof exact-integer?))]
          ))

(define (decode 
         raw_list
         patrity_length
         #:bit_width [bit_width 8]
         #:primitive_poly_value [primitive_poly_value 285]
         #:express? [express? #f]
         #:express_path [express_path ".decode.express"])

  (parameterize*
   ([*express?* express?]
    [*express_path* express_path]
    [*bit_width* bit_width]
    [*2^m_1* (sub1 (expt 2 (*bit_width*)))]
    [*primitive_poly_value* primitive_poly_value]
    [*gf_aton_map* (get-gf-aton-hash)]
    [*gf_ntoa_map* (make-hash (hash-map (*gf_aton_map*) (lambda (a n) (cons n a))))])

   (let (
         [t #f]
         [syndromes #f]
         [syndrome_poly #f]
         [lam_poly #f]
         [ome_poly #f]
         [err_places #f]
         [err_correct_pairs #f]
         [corrected_values #f]
         )
     
     (set! t (floor (/ patrity_length 2)))

     (set! syndromes (get-syndromes raw_list (* 2 t)))
     
     (set! syndrome_poly (coeffients->poly-n syndromes))
     
     (let-values ([(ome lam) (error-locator-poly syndrome_poly t)])
       (set! ome_poly ome)
       (set! lam_poly lam))
     
     (set! err_places (chien-search lam_poly))
     
     (set! err_correct_pairs
           (map
            (lambda (err_place)
              (cons
               err_place
               (forney lam_poly ome_poly err_place)))
            err_places))
     
     (set! corrected_values 
           (correct-error 
            raw_list
            err_correct_pairs))

     corrected_values)))
