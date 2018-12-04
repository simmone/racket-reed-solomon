#lang racket

(require "poly.rkt")
(require "generator-poly.rkt")
(require "long-division.rkt")

(provide (contract-out
          [encode (-> string? natural? natural? (listof natural?))]
          ))

(define (encode data patrity_length bit_width)
  (let* ([gf_hash (get-gf-hash bit_width)]
         [aton_map (car gf_hash)]
         [ntoa_map (cdr gf_hash)]
         [generator_poly (generator-poly patrity_length)]
         [message_poly (message->poly data)]
         [message_length (string-length data)])
    
    (let loop ([loop_message_n (prepare-message message_poly patrity_length)]
               [count 1])
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
            
            (set! step3_get_first_a (hash-ref ntoa_map (caar (string->poly loop_message_n))))
            
            (set! step4_multiply_a (poly-multiply step2_aligned_generator_a (format "a~a" step3_get_first_a)))
            
            (set! step5_to_n (poly-a->n step4_multiply_a))
            
            (set! step6_xor (poly-n-xor loop_message_n step5_to_n))

            (set! step7_discard_first (poly-cdr step6_xor))
            
            (loop step7_discard_first (add1 count)))
          (map
           (lambda (pair)
             (car pair))
           (string->poly loop_message_n))))))
