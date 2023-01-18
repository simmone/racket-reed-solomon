#lang racket

(provide (contract-out
          [get-field-table (-> natural? string? hash?)]
          [poly_a->index_list (-> string? (listof natural?))]
          [index_list->poly_a (-> (listof natural?) string?)]
          [poly_a-multiply-n (-> string? natural? string?)]
          [poly_a->n (-> string? natural?)]
          ))

(define (get-field-table bit_width field_generator_poly)
  (make-hash))

(define (poly_a->index_list poly_a)
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
  (let loop ([indexes (sort index_list >)]
             [last_result ""]
             [last_operator ""])
    (if (not (null? indexes))
        (loop
         (cdr indexes)
         (format "~a~a~a"
                 last_result
                 last_operator
                 (if (= (car indexes) 0)
                     "1"
                     (format "a~a" (car indexes))))
           "+")
        last_result)))

(define (poly_a-multiply-n poly n)
  (index_list->poly_a
   (map
    (lambda (index)
      (+ index n))
    (poly_a->index_list poly))))  

(define (poly_a->n poly)
  (foldl + 0
         (map
          (lambda (index)
            (expt 2 index))
          (poly_a->index_list poly))))
