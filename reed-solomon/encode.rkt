#lang racket

(require "lib/poly.rkt")
(require "lib/generator-poly.rkt")
(require "lib/long-division.rkt")

(require "lib/lib.rkt")
(require "lib/encode-express/express.rkt")

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
     
     (printf "start division~a\n" message_length)

     (let loop ([loop_message_n (prepare-message message_poly patrity_length)]
                [count 1])
       
       (printf "~a\n" loop_message_n)

       (express express? (lambda () (write-report-long-division-prepare-message count loop_message_n express_path)))

       (if (<= count message_length)
           (let* ([step1_aligned_message_x_length #f]
                  [step2_aligned_generator_a  #f]
                  [step3_get_first_a #f]
                  [step4_multiply_a #f]
                  [step5_to_n #f]
                  [step6_xor_n #f]
                  [step7_discard_first_zeros_n #f]
                  [step8_zeros_count #f])

             (set! step1_aligned_message_x_length (cdar (string->poly loop_message_n)))
             
             (printf "count: ~a\n" count)
             
             (set! step2_aligned_generator_a (prepare-generator generator_poly step1_aligned_message_x_length))

             (printf "aligned_generator: ~a\n" step2_aligned_generator_a)
             
             (set! step3_get_first_a (hash-ref (*gf_ntoa_map*) (caar (string->poly loop_message_n))))

             (printf "first a: ~a\n" step3_get_first_a)
             
             (set! step4_multiply_a (poly-multiply step2_aligned_generator_a (format "a~a" step3_get_first_a) ))

             (printf "aligned_generator: ~a\n" step4_multiply_a)
             
             (set! step5_to_n (poly-a->n step4_multiply_a))

             (printf "generator_n: ~a\n" step5_to_n)

             (printf "message_n: ~a\n" loop_message_n)
             
             (set! step6_xor_n (poly-n-xor loop_message_n step5_to_n))

             (printf "xor: ~a\n" step6_xor_n)

             (set! step7_discard_first_zeros_n 
                   (regexp-replace* #rx"a" 
                                    (poly->string
                                     (let loop ([loop_list (string->poly step6_xor_n)])
                                       (if (= (caar loop_list) 0)
                                           (loop (cdr loop_list))
                                           loop_list)))
                                    ""))
             
             (printf "cut first zeros: ~a\n" step7_discard_first_zeros_n)
             
             (set! step8_zeros_count (- (length (string->poly loop_message_n)) (length (string->poly step7_discard_first_zeros_n))))

             (printf "zero count: ~a\n" step8_zeros_count)
             
             (loop step7_discard_first_zeros_n (+ count step8_zeros_count)))
           (map
            (lambda (pair)
              (car pair))
            (string->poly loop_message_n)))))))
