#lang racket

(provide (contract-out
          [display-list (->* (list?) (natural? natural?) void?)]
          [print-line (-> list? void?)]
          [print-divide-elements (-> list? void?)]
          ))

(define (print-divide-elements elements)
  (let loop ([loop_elements elements])
    (when (not (null? loop_elements))
      (printf "[~a]" (~a #:min-width 3 #:align 'left #:right-pad-string " " (car loop_elements)))
      (loop (cdr loop_elements))))
  (printf "\n"))

(define (print-line item_list)
  (let loop ([items item_list])
    (if (not (null? items))
        (begin
          (printf "~a|" (~a #:min-width 2 #:align 'left #:right-pad-string " " (car items)))
          (loop (cdr items)))
        (printf "\n"))))

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
