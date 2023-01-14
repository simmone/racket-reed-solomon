#lang racket

(require "../lib/gf.rkt")

(provide (contract-out
          [get-syndromes (-> (listof exact-integer?) natural? (listof exact-integer?))]
          ))

(define (get-syndromes raw_list error_code_length)
  (let loop ([loop_a_count 0]
             [result_list '()])
    (if (< loop_a_count error_code_length)
        (begin
          (loop
           (add1 loop_a_count)
           (cons
            (let xor-loop ([loop_n_list raw_list]
                           [last_result_n 0])
              (if (> (length loop_n_list) 1)
                  (let ([ri (car loop_n_list)]
                        [gf+ #f]
                        [ntoa #f]
                        [a+ #f]
                        [aton #f])

                    (set! gf+ (bitwise-xor last_result_n (car loop_n_list)))

                    (set! ntoa (hash-ref (*gf_ntoa_map*) gf+ 0))

                    (if (= gf+ 0)
                        (begin
                          (set! a+ 0)
                          (set! aton 0))
                        (begin
                          (set! a+ (+ loop_a_count ntoa))

                          (set! aton (hash-ref (*gf_aton_map*) (modulo a+ (*2^m_1*))))))
                    
                    (xor-loop (cdr loop_n_list) aton))
                  (let ([result (bitwise-xor last_result_n (car loop_n_list))])
                    result)))
            result_list)))
        result_list)))

