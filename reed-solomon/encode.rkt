#lang racket

(require "lib/share/lib.rkt")
(require "lib/share/gf.rkt")
(require "lib/share/poly.rkt")
(require "lib/share/euclidean.rkt")
(require "lib/encode/generator-poly.rkt")
(require "lib/encode/express/express.rkt")

(provide (contract-out
          [encode (->* 
                   ((listof exact-integer?) natural?) 
                   (#:bit_width natural? #:primitive_poly_value natural? #:express? boolean? #:express_path path-string?) 
                   (listof exact-integer?))]
          ))

(define (encode 
         raw_list
         patrity_length
         #:bit_width [bit_width 8]
         #:primitive_poly_value [primitive_poly_value 285]
         #:express? [express? #f]
         #:express_path [express_path ".encode.express"])

  (when express?
        (delete-directory/files #:must-exist? #f express_path)
        (make-directory* express_path))

  (express express? (lambda () (write-report-header express_path)))

  (express express?
           (lambda () (write-report-input raw_list patrity_length bit_width primitive_poly_value express_path)))

  (express express? (lambda () (write-report-primitive-poly express_path)))

  (parameterize*
   ([*bit_width* bit_width]
    [*2^m_1* (sub1 (expt 2 (*bit_width*)))]
    [*primitive_poly_value* primitive_poly_value]
    [*gf_aton_map* (get-gf-aton-hash)]
    [*gf_ntoa_map* (make-hash (hash-map (*gf_aton_map*) (lambda (a n) (cons n a))))])

   (let* ([generator_poly (generator-poly patrity_length)]
          [message_poly (coeffients->poly-n raw_list)]
          [message_length (length raw_list)])

     (express express? (lambda () (write-report-galois-fields (*gf_aton_map*) (*gf_ntoa_map*) express_path)))

     (express express? (lambda () (write-report-generator-poly generator_poly express_path)))

     (express express? (lambda () (write-report-message-poly message_poly express_path)))

     (let-values ([(quotient remainder) 
                   (euc-divide 
                    (poly-gf-n-multiply message_poly (format "x~a" patrity_length))
                    (poly-gf-a->n generator_poly))])

       (let ([result
              (map
               (lambda (pair)
                 (car pair))
               (string-n->poly remainder))])

       (express express? (lambda () (write-report-error-code bit_width result express_path)))
       
       result)))))
       
