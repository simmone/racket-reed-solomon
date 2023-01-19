#lang racket

(provide (contract-out
          [get-field-table (-> natural? string? hash?)]
          [poly->indexes (-> string? (listof natural?))]
          [indexes->poly (-> (listof natural?) string?)]
          [poly-multiply-n (-> string? natural? string?)]
          [poly->sum (-> string? natural?)]
          [poly->equal_pair (-> string? (cons/c string? string?))]
          [poly-remove_dup (-> string? string?)]
          [poly-multiply-poly (-> string? string? string?)]
          [poly->coefficients (-> string? (listof (or/c 0 1)))]
          ))

(define (get-field-table bit_width field_generator_poly)
  (make-hash))

(define (poly->indexes poly)
  (let loop ([loop_list (regexp-split #rx"\\+" poly)]
             [result_list '()])
    (if (not (null? loop_list))
        (loop
         (cdr loop_list)
         (cons
          (let ([a_item (string-trim (car loop_list))])
            (cond
             [(string=? a_item "1") 0]
             [(string=? a_item "a") 1]
             [else
              (string->number (second (regexp-split #rx"a" a_item)))]))
          result_list))
        (sort (reverse result_list) >))))

(define (indexes->poly indexes)
  (let loop ([indexes (sort indexes >)]
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

(define (poly-multiply-n poly n)
  (indexes->poly
   (map
    (lambda (index)
      (+ index n))
    (poly->indexes poly))))  

(define (poly->sum poly)
  (foldl + 0
         (map
          (lambda (index)
            (expt 2 index))
          (poly->indexes poly))))

(define (poly->equal_pair poly)
  (let ([indexes (poly->indexes poly)])
    (cons
     (indexes->poly (list (car indexes)))
     (indexes->poly (cdr indexes)))))

(define (poly-remove_dup poly)
  (indexes->poly
   (let loop ([indexes (poly->indexes poly)]
              [result_list '()])
     (if (not (null? indexes))
         (if (member (car indexes) result_list)
             (loop
              (cdr indexes)
              (remove (car indexes) result_list))
             (loop
              (cdr indexes)
              (cons (car indexes) result_list)))
         (reverse result_list)))))

(define (poly-multiply-poly poly1_a poly2_a)
  (poly-remove_dup
   (indexes->poly
    (let loop-poly1 ([poly1_indexes (poly->indexes poly1_a)]
                     [poly1_result '()])
      (if (not (null? poly1_indexes))
          (loop-poly1
           (cdr poly1_indexes)
           (cons
            (poly->indexes
             (poly-multiply-n poly2_a (car poly1_indexes)))
            poly1_result))
          (sort (flatten poly1_result) <))))))

(define (poly->coefficients poly)
  (let ([indexes (poly->indexes poly)])
    (let loop ([index (car indexes)]
               [coefficient_list '()])
      (if (>= index 0)
          (loop
           (sub1 index)
           (cons
            (if (member index indexes) 1 0)
            coefficient_list))
          (reverse coefficient_list)))))
