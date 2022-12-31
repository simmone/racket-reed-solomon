#lang racket

(require rackunit "../lib/encode/encode.rkt")

(define (display-list input_list [col_width 12] [line_count 10])
  (let loop ([loop_list input_list]
             [item_count 1])
    (when (not (null? loop_list))
      (let ([item (~a #:min-width col_width #:align 'left #:right-pad-string " " (format "[~a]" (car loop_list)))])
        (if (<= item_count line_count)
            (begin
              (printf "~a" item)
              
              (loop (cdr loop_list) (add1 item_count)))
            (begin
              (printf "\n~a" item)

              (loop (cdr loop_list) 2))))))
  (printf "\n\n"))

;;    (check-equal?
;;     (rs-encode '(32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17) 10)
;;     (list 196  35  39  119  235  215  231  226  93  23))
    
(let* ([input_data_list '(32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17)]
       [parity_length 10]
       [bit_width 8]
       [2^m_1 (sub1 (expt 2 bit_width))])

  (printf "start enode\n\n")

  (printf "input data list:\n\n")
  
  (display-list input_data_list)
  
  (printf "parity_length:~a\n\n" parity_length)

  (printf "bit_length:~a\n\n" bit_width)

  (printf "2^m-1:~a\n\n" 2^m_1)
  )
