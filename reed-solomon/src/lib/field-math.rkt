#lang racket

(provide (contract-out
          [get-field-table (-> natural? string? hash?)]
          [poly-a->index_list (-> string? (listof natural?))]
          [index_list->poly_a (-> (listof natural?) string?)]
          [poly-a-multiply-a (-> string? string? string?)]
          ))

(define (get-field-table bit_width field_generator_poly)
  (make-hash))

(define (poly-a->index_list poly_a)
  (let loop ([loop_list (regexp-split #rx"\\+" poly_a)]
             [result_list '()])
    (if (not (null? loop_list))
        (loop
         (cdr loop_list)
         (cons
          (let ([a_item (string-trim (car loop_list))])
            (cond
             [(string=? a_item "1") 0]
             [else
              (string->number (second (regexp-split #rx"a" a_item)))]))
          result_list))
        (sort (reverse result_list) >))))

(define (index_list->poly_a index_list)
  (let loop ([indexes index_list]
             [result_string ""])
    (if (not (null? indexes))
        (begin
          (printf "indexes:~a: result_string:~a\n" indexes result_string)
          (loop
           (cdr indexes)
           (format "~a~a~a"
                   result_string
                   (if (null? (cdr indexes)) "" "+")
                   (if (= (car indexes) 0) "1" (format "a~a" (car indexes))))))
          result_string)))

(define (poly-a-multiply-a poly1 poly2)
  "")
  
