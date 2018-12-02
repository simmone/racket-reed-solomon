#lang racket

(require "poly.rkt")
(require "generator-poly.rkt")
(require "long-division.rkt")

(provide (contract-out
          [encode (-> string? natural? (listof natural?))]
          ))

(define (encode data patrity_length)
  (let* ([gf256_hash (get-gf256-hash)]
         [aton_map (car gf256_hash)]
         [ntoa_map (cdr gf256_hash)]
         [generator_poly (generator-poly patrity_length)]
         [message_poly (message->poly data)]
         [message_length (string-length data)]
         [aligned_message_n (prepare-message message_poly patrity_length)]
         [aligned_message_x_length (cdar (string->poly (poly-car aligned_message_n)))]
         [aligned_generator_a (prepare-generator generator_poly aligned_message_x_length)])
    
    (let loop ([loop_generator_a aligned_generator_a]
               [loop_message_n aligned_message_n])
      (if (not (string=? loop_generator_a ""))
          (let ([step1_get_first_a #f]
                [step2_multiply_a #f]
                [step3_to_n #f]
                [step4_xor #f]
                [step5_discard_first #f])

            (printf "start\n")

            (printf "generator:~a\n" loop_generator_a)
            
            (printf "message:~a\n" loop_message_n)

            (set! step1_get_first_a (hash-ref ntoa_map (caar (string->poly loop_message_n))))
            
            (printf "first_a: ~a\n" step1_get_first_a)
            
            (set! step2_multiply_a (poly-multiply loop_generator_a (format "a~a" step1_get_first_a)))
            
            (printf "multiply_a: ~a\n" step2_multiply_a)
            
            (set! step3_to_n (poly-a->n step2_multiply_a))
            
            (printf "to_n: ~a\n" step3_to_n)
            
            (set! step4_xor (poly-n-xor loop_message_n (poly-a->n step2_multiply_a)))
            
            (printf "xor: ~a\n" step4_xor)
            
            (set! step5_discard_first (poly-cdr step4_xor))
            
            (printf "end:~a\n" step5_discard_first)
            
            (loop (poly-n->a step5_discard_first) (poly-cdr loop_message_n)))
          (map
           (lambda (pair)
             (car pair))
           (string->poly (poly-a->n loop_generator_a)))))))
