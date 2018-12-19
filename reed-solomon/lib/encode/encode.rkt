#lang racket

(require "../share/lib.rkt")
(require "../share/gf.rkt")
(require "../share/poly.rkt")
(require "../share/euclidean.rkt")

(require "generator-poly.rkt")
(require "express/express.rkt")

(provide (contract-out
          [rs-encode (->* 
                      ((listof exact-integer?) natural?) 
                      (#:bit_width natural? #:primitive_poly_value natural? #:express? boolean? #:express_path path-string?) 
                      (listof exact-integer?))]
          ))

(define (rs-encode 
         raw_list
         patrity_length
         #:bit_width [bit_width 8]
         #:primitive_poly_value [primitive_poly_value 285]
         #:express? [express? #f]
         #:express_path [express_path ".encode.express"])

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

         (let* ([generator_poly (generator-poly patrity_length)]
                [message_poly (coeffients->poly-n raw_list)]
                [message_length (length raw_list)])
           
           (express-galois-fields (*gf_aton_map*) (*gf_ntoa_map*))

           (express-generator-poly generator_poly)

           (express-message-poly message_poly)

           (let-values ([(quotient remainder) 
                         (euc-divide 
                          (poly-gf-n-multiply message_poly (format "x~a" patrity_length))
                          (poly-gf-a->n generator_poly)
                          #t)])

             (let ([result (poly-n->coeffients remainder)])
               (express-error-code bit_width result)
               
               result))))
       (lambda ()
         (express-primitive-poly)))))


   