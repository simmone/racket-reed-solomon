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

  (let* ([gf_hash (get-gf-hash bit_width primitive_poly_value)]
         [aton_map (car gf_hash)]
         [ntoa_map (cdr gf_hash)]
         [generator_poly (generator-poly patrity_length aton_map ntoa_map)]
         [message_poly (message->poly raw_list)]
         [message_length (length raw_list)])

    (express express? (lambda () (write-report-galois-fields aton_map express_path)))

    (express express? (lambda () (write-report-generator-poly generator_poly express_path)))

    (express express? (lambda () (write-report-message-poly message_poly express_path)))

    (express express? (lambda () (write-report-long-division-start express_path)))
    
    (printf "start division\n")

    (let loop ([loop_message_n (prepare-message message_poly patrity_length aton_map ntoa_map)]
               [count 1])
      
      (printf "~a\n" loop_message_n)

      (express express? (lambda () (write-report-long-division-prepare-message count loop_message_n express_path)))

      (if (<= count message_length)
          (let* ([step1_aligned_message_x_length #f]
                 [step2_aligned_generator_a  #f]
                 [step3_get_first_a #f]
                 [step4_multiply_a #f]
                 [step5_to_n #f]
                 [step6_xor #f]
                 [step7_discard_first #f])

            (set! step1_aligned_message_x_length (cdar (string->poly (poly-car loop_message_n))))
            
            (set! step2_aligned_generator_a (prepare-generator generator_poly step1_aligned_message_x_length))

            (printf "~a\n" step2_aligned_generator_a)
            
            (set! step3_get_first_a (hash-ref ntoa_map (caar (string->poly loop_message_n))))

            (printf "~a\n" step3_get_first_a)
            
            (set! step4_multiply_a (poly-multiply step2_aligned_generator_a (format "a~a" step3_get_first_a) ))

            (printf "~a\n" step4_multiply_a)
            
            (set! step5_to_n (poly-a->n step4_multiply_a aton_map))

            (printf "~a\n" step5_to_n)
            
            (set! step6_xor (poly-n-xor loop_message_n step5_to_n))

            (set! step7_discard_first (poly-cdr step6_xor))
            
            (loop step7_discard_first (add1 count)))
          (map
           (lambda (pair)
             (car pair))
           (string->poly loop_message_n))))))
