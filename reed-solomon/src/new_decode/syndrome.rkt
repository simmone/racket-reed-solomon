#lang racket

(require "../field-math.rkt")

(provide (contract-out
          [get-syndromes (-> (listof natural?) natural? (listof natural?))]
          ))

(define (get-syndromes data_list parity_length)
  (let loop ([loop_parity_index 0]
             [result_list '()])
    (let* ([ax (format "a~a" loop_parity_index)]
           [ax_val (hash-ref (*galios_index->number_map*) ax)])

      (if (< loop_parity_index parity_length)
          (loop
           (add1 loop_parity_index)
           (cons
            (let step-loop ([loop_data_list data_list]
                            [last_result 0]
                            [last_xor_result #f])
              (if (not (null? loop_data_list))
                  (let ([last_xor #f]
                        [ax_multiply #f])

                    (set! last_xor (bitwise-xor last_result (car loop_data_list)))

                    (set! ax_multiply (galios-multiply last_xor ax_val))

                    (step-loop (cdr loop_data_list) ax_multiply last_xor))
                  last_xor_result))
            result_list))
          result_list))))

