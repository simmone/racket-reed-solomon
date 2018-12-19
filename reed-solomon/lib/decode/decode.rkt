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
               [lam_poly #f]
               [ome_poly #f]
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
                    (express-too-many-errors)
                    raw_list)])
                (let-values ([(ome lam) (error-locator-poly syndrome_poly t #t)])
                  (set! ome_poly ome)
                  (set! lam_poly lam)
                  
                  (set! err_places (chien-search lam_poly))
                  
                  (express-chien-search err_places)

                  (set! lam_derivative_poly (derivative-lam lam_poly))

                  (let-values ([(quotient remainder) (euc-divide ome_poly lam_derivative_poly)])
                    (printf "remainder:~a\n" remainder)

                    (set! Yj_poly quotient))

                  (set! err_correct_pairs
                        (map
                         (lambda (err_place)
                           (cons
                            err_place
                            (forney Yj_poly err_place)))
                         err_places))

                  (express-forney lam_poly ome_poly lam_derivative_poly Yj_poly err_correct_pairs)

                  (set! corrected_values 
                        (correct-error 
                         raw_list
                         err_correct_pairs))

                  (express-finally corrected_values bit_width)

                  corrected_values)))))
       (lambda ()
         (express-primitive-poly)

         (express-euclidean-decode)))))



