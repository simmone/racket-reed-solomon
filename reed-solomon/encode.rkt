#lang racket

(require "lib/share/lib.rkt")
(require "lib/share/gf.rkt")
(require "lib/share/poly.rkt")
(require "lib/encode/generator-poly.rkt")
(require "lib/encode/long-division.rkt")
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
          [message_poly (message->poly raw_list)]
          [message_length (length raw_list)])

     (express express? (lambda () (write-report-galois-fields (*gf_aton_map*) (*gf_ntoa_map*) express_path)))

     (express express? (lambda () (write-report-generator-poly generator_poly express_path)))

     (express express? (lambda () (write-report-message-poly message_poly express_path)))

     (express express? (lambda () (write-report-long-division-start express_path)))
     
     (let loop ([loop_message_n (prepare-message message_poly patrity_length)]
                [count 1])
       
       (if (<= count message_length)
           (let* ([step1_message_x #f]
                  [step2_message_n #f]
                  [step3_aligned_generator_by_x #f]
                  [step4_message_n_to_a #f]
                  [step5_multiply_a #f]
                  [step6_to_n #f]
                  [step7_xor_n #f]
                  [step8_discard_first_zeros_n #f]
                  [step9_zeros_count #f])

             (set! step1_message_x (cdar (string->poly loop_message_n)))

             (set! step2_message_n (caar (string->poly loop_message_n)))

             (set! step3_aligned_generator_by_x (prepare-generator generator_poly step1_message_x))

             (set! step4_message_n_to_a (hash-ref (*gf_ntoa_map*) step2_message_n))

             (set! step5_multiply_a (poly-gf-a-multiply step3_aligned_generator_by_x (format "a~a" step4_message_n_to_a)))

             (set! step6_to_n (poly-gf-a->n step5_multiply_a))

             (set! step7_xor_n (poly-n-xor loop_message_n step6_to_n))

             (set! step8_discard_first_zeros_n 
                   (regexp-replace* #rx"a" 
                                    (poly->string
                                     (let loop ([loop_list (string->poly step7_xor_n)])
                                       (if (= (caar loop_list) 0)
                                           (loop (cdr loop_list))
                                           loop_list)))
                                    ""))
             
             (set! step9_zeros_count (- (length (string->poly loop_message_n)) (length (string->poly step8_discard_first_zeros_n))))

             (express express? (lambda () 
                                 (write-report-long-division-detail
                                  count
                                  generator_poly
                                  loop_message_n
                                  step1_message_x
                                  step2_message_n
                                  step3_aligned_generator_by_x
                                  step4_message_n_to_a
                                  step5_multiply_a
                                  step6_to_n
                                  step7_xor_n
                                  step8_discard_first_zeros_n
                                  step9_zeros_count
                                  express_path)))
             
             (loop step8_discard_first_zeros_n (+ count step9_zeros_count)))
           (let ([result
                  (map
                   (lambda (pair)
                     (car pair))
                   (string->poly loop_message_n))])
             (express express? (lambda () (write-report-error-code bit_width result express_path)))
             result))))))
