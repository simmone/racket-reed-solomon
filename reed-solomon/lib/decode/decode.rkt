#lang racket

(require "../share/lib.rkt")
(require "../share/gf.rkt")
(require "../share/poly.rkt")
(require "../share/euclidean.rkt")

(require "syndrome.rkt")
(require "error-locator-poly.rkt")
(require "chien-search.rkt")
(require "forney.rkt")
(require "correct-error.rkt")

(require "express/express.rkt")

(provide (contract-out
          [rs-decode (->* 
                      ((listof exact-integer?) natural?) 
                      (#:bit_width natural? #:primitive_poly_value natural? #:express? boolean? #:express_path path-string?) 
                      (listof exact-integer?))]
          ))

(define (rs-decode 
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

   (dynamic-wind
       (lambda ()
         (express-start))
       (lambda ()
         (express-input raw_list patrity_length bit_width primitive_poly_value)

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
           
           (set! t (floor (/ patrity_length 2)))

           (set! syndromes (get-syndromes raw_list (* 2 t) #t))

           (set! syndrome_poly (coeffients->poly-n syndromes))
           
           (express-syndrome-poly syndrome_poly)
           
           (if (= (foldr + 0 syndromes) 0)
               (begin
                 (express-no-error)
                 raw_list)
               (with-handlers
                ([exn:fail?
                  (lambda (v)
                    (express-too-many-errors v)
                    raw_list)])
                (let-values ([(ome_poly lam_poly) (error-locator-poly syndrome_poly t #t)])
                  (set! err_places (chien-search lam_poly))
                  
                  (express-chien-search err_places)

                  (set! err_correct_pairs (forney lam_poly ome_poly err_places #t))

                  (set! corrected_values 
                        (correct-error 
                         raw_list
                         err_correct_pairs))

                  (express-finally corrected_values bit_width)

                  corrected_values)))))
       (lambda ()
         (express-galois-fields (*gf_aton_map*) (*gf_ntoa_map*))

         (express-primitive-poly)

         (express-euclidean-decode)))))



