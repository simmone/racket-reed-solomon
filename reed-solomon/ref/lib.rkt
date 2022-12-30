#lang racket

(provide (contract-out
          [display-list (->* (list?) (natural? natural?) void?)]
          ))

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
