#lang racket

(provide (contract-out
          [get-field-table (-> natural? string? hash?)]
          [poly->num_index_pairs (-> string? (listof (cons/c natural? natural?)))]
          [num_index_pairs->poly (-> (listof (cons/c natural? natural?)) string?)]
          [poly-multiply (-> string? string? string?)]
          [poly-sum (-> string? natural?)]
          [poly->equal_pair (-> string? (cons/c string? string?))]
          [poly-remove_dup (-> string? string?)]
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
        [poly_multiplier_pairs (poly->num_index_pairs poly_multiplier)])
    
    (let loop-multiplicand ([loop_poly_multiplicand_pairs poly_multiplicand_pairs]
                            [multiplicand_list '()])
      (if (not (null? loop_poly_multiplicand_pairs))
          (loop-multiplicand
           (cdr loop_poly_multiplicand_pairs)
           (cons
            (num_index_pairs->poly
             (let loop-multiplier ([loop_poly_multiplier_pairs poly_multiplier_pairs]
                                   [multiplier_list '()])
               (if (not (null? loop_poly_multiplier_pairs))
                   (loop-multiplier
                    (cdr loop_poly_multiplier_pairs)
                    (cons
                     (cons
                      (* (caar loop_poly_multiplicand_pairs) (caar loop_poly_multiplier_pairs))
                      (+ (cdar loop_poly_multiplicand_pairs) (cdar loop_poly_multiplier_pairs)))
                     multiplier_list))
                   (reverse multiplier_list))))
            multiplicand_list))
          (num_index_pairs->poly
           (let ([combine_hash (make-hash)])
             (let loop ([loop_polys multiplicand_list])
               (when (not (null? loop_polys))
                 (map
                  (lambda (p)
                    (if (hash-has-key? combine_hash (cdr p))
                        (hash-set! combine_hash (cdr p) (+ (hash-ref combine_hash (cdr p)) (car p)))
                        (hash-set! combine_hash (cdr p) (car p))))
                  (poly->num_index_pairs (car loop_polys)))
                 (loop (cdr loop_polys))))
             (hash-map combine_hash (lambda (a n) (cons n a)))))))))

(define (poly-sum poly)
  (let loop ([pairs (poly->num_index_pairs poly)]
             [sum 0])
    (if (not (null? pairs))
        (loop (cdr pairs) (+ sum (* 2 (cdar pairs))))
        sum)))

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
