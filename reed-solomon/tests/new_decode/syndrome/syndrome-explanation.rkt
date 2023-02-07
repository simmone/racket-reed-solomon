#lang racket

(require "../../../src/field-math.rkt")

(require rackunit)

(define (_get-syndromes data_list parity_length)
  (printf "Get Syndromes Explanation:\n\n")

  (let loop ([loop_parity_index 0]
             [result_list '()])

    (printf "result_list: ~a\n" result_list)

    (let* ([ax (format "a~a" loop_parity_index)]
           [ax_val (hash-ref (*galios_index->number_map*) ax)])

      (printf "~a = ~a\n" ax ax_val)

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

                    (printf "( ~a bitwise-xor ~a ) = ~a galios-multiply ~a = ~a\n"
                            (~a #:min-width 3 #:align 'left #:right-pad-string " " last_result)
                            (~a #:min-width 3 #:align 'left #:right-pad-string " " (car loop_data_list))
                            (~a #:min-width 3 #:align 'left #:right-pad-string " " last_xor)
                            (~a #:min-width 3 #:align 'left #:right-pad-string " " ax_val)
                            (~a #:min-width 3 #:align 'left #:right-pad-string " " ax_multiply))

                    (step-loop (cdr loop_data_list) ax_multiply last_xor))
                  last_xor_result))
            result_list))
          (let trim-loop ([items result_list])
            (if (not (null? items))
                (if (= (car items) 0)
                    (trim-loop (cdr items))
                    items)
                items))))))

(parameterize*
 ([*bit_width* 4]
  [*field_generator_poly* "x4+x+1"]
  [*galios_index->number_map* (get-galios-index->number_map (*bit_width*))])

    (parameterize*
     ([*bit_width* 4]
      [*field_generator_poly* "x4+x+1"]
      [*galios_index->number_map* (get-galios-index->number_map (*bit_width*))])

;     (check-equal? (_get-syndromes 
;                    '(1 2 3 4 5 11 7 8 9 10 11 3 1 12 12) 4)
;                   '(12 4 3 15))

     (check-equal? (_get-syndromes
                    '(12 12 1 3 11 10 9 8 7 6 5 4 3 2 1) 4)
                   '(7 14 2))

     ))



