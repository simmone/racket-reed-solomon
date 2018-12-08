#lang racket

(provide (contract-out
          [get-syndromes (-> (listof exact-integer?) natural? (listof exact-integer?))]
          ))

(define (get-syndromes raw_list error_code_length)
  (let loop ([loop_count 0]
             [result_list '()])
    (if (< loop_count 14)
        (loop
         (add1 loop_count)
         (cons
          (let xor-loop ([loop_list raw_list]
                         [last_result loop_count])
            (if (not (null? loop_list))
                (xor-loop (cdr loop_list) (bitwise-xor last_result (car loop_list)))
                last_result))
          result_list))
        result_list)))
          
