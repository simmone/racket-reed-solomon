#lang racket

(require "../share/gf.rkt")

(provide (contract-out
          [get-syndromes (-> (listof exact-integer?) natural? (listof exact-integer?))]
          ))

(define (get-syndromes raw_list error_code_length)
  (let loop ([loop_a_count 3]
             [result_list '()])
    (if (< loop_a_count error_code_length)
        (loop
         (add1 loop_a_count)
         (cons
          (let xor-loop ([loop_n_list raw_list]
                         [last_result_n 0])
            (if (not (null? loop_n_list))
                (let ([ri (car loop_n_list)]
                      [gf+ #f]
                      [ntoa #f]
                      [a+ #f]
                      [aton #f])

                  (set! gf+ (bitwise-xor last_result_n (car loop_n_list)))

                  (set! ntoa (hash-ref (*gf_ntoa_map*) gf+ 0))

                  (if (= ntoa 0)
                      (begin
                        (set! a+ 0)
                        (set! aton 0))
                      (begin
                        (set! a+ (+ loop_a_count ntoa))

                        (set! aton (hash-ref (*gf_aton_map*) (modulo a+ (*2^m_1*))))))

                  (printf "last_n:[~a], ri:[~a], gf+:[~a], ntoa:[~a], a+:[~a], aton:[~a]\n" last_result_n ri gf+ ntoa a+ aton)

                  (xor-loop (cdr loop_n_list) aton))
                last_result_n))
          result_list))
        result_list)))
          
