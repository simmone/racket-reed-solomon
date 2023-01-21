#lang racket

(provide (contract-out
          [get-field-table (-> natural? string? hash?)]
          [poly->num_index_pairs (-> string? (listof (cons/c natural? natural?)))]
          [num_index_pairs->poly (-> (listof (cons/c natural? natural?)) string?)]
          [poly-multiply (-> string? string? string?)]
          [poly->sum (-> string? natural?)]
          [poly->equal_pair (-> string? (cons/c string? string?))]
          [poly-remove_dup (-> string? string?)]
          [poly-multiply-poly (-> string? string? string?)]
          [poly->coefficients (-> string? string?)]
          ))

(define (get-field-table bit_width field_generator_poly)
  (make-hash))

(define (poly->num_index_pairs poly)
  (let loop ([loop_list (regexp-split #rx"\\+" poly)]
             [result_list '()])
    (if (not (null? loop_list))
        (loop
         (cdr loop_list)
         (cons
          (let ([items (regexp-split #rx"\\x" (string-trim (car loop_list)))])
            (cond
             [(and (= (length items) 1) (string=? (first items) "1"))
              '(1 . 0)]
             [(= (length items) 2)
              (cons 1
                    (if (string=? (second items) "")
                        1
                        (string->number (second items))))]
             [(= (length items) 3)
              (cons (first items) (string->number (second items)))]
             [else
              (error (format "invalid poly: [~a]" (string-trim (car loop_list))))]))
          result_list))
        (sort (reverse result_list) > #:key cdr))))

(define (num_index_pairs->poly pairs)
  (let loop ([loop_pairs (sort pairs > #:key cdr)]
             [last_result ""]
             [last_operator ""])
    (if (not (null? loop_pairs))
        (loop
         (cdr loop_pairs)
         (format "~a~a~a"
                 last_result
                 last_operator
                 (if (= (cdar loop_pairs) 0)
                     "1"
                     (format "~ax~a"
                             (if (= (caar loop_pairs) 1) "" (caar loop_pairs))
                             (if (= (cdar loop_pairs) 1) "" (cdar loop_pairs)))))
           "+")
        last_result)))

(define (poly-multiply poly_multiplicand poly_multiplier)
  (let ([poly_multiplicand_pairs (poly->num_index_pairs poly_multiplicand)]
        [poly_mulitiplier_pairs (poly->num_index_pairs poly_multiplier)])
    
    (let loop-multiplier ([loop_poly_multiplicand_pairs poly_multiplicand_pairs]
                          [result_list '()])
      (if (not (null? loop_poly_multiplicand_pairs))


(define (poly->equal_pair poly)
  (let ([indexes (poly->num_index_pairs poly)])
    (cons
     (num_index_pairs->poly (list (car indexes)))
     (num_index_pairs->poly (cdr indexes)))))

(define (poly-remove_dup poly)
  (num_index_pairs->poly
   (let loop ([indexes (poly->num_index_pairs poly)]
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
   (num_index_pairs->poly
    (let loop-poly1 ([poly1_indexes (poly->num_index_pairs poly1_a)]
                     [poly1_result '()])
      (if (not (null? poly1_indexes))
          (loop-poly1
           (cdr poly1_indexes)
           (cons
            (poly->num_index_pairs
             (poly-multiply-n poly2_a (car poly1_indexes)))
            poly1_result))
          (sort (flatten poly1_result) <))))))

(define (poly->coefficients poly)
  (with-output-to-string
    (lambda ()
      (let ([indexes (poly->num_index_pairs poly)])
        (let loop ([index (car indexes)])
          (when (>= index 0)
            (if (member index indexes)
                (printf "1")
                (printf "0"))
            (loop (sub1 index))))))))
