#lang racket

(provide (contract-out
          [get-field-table (-> natural? string? hash?)]
          [poly->index_coe_pairs (-> string? (listof (cons/c natural? natural?)))]
          [index_coe_pairs->poly (-> (listof (cons/c natural? natural?)) string?)]
          [poly-multiply (->* (string? string?) (boolean?) string?)]
          [galios-poly-multiply (-> string? string? string?)]
          [poly-sum (-> string? natural?)]
          [poly->equal_pair (-> string? (cons/c string? string?))]
          [poly-remove_dup (-> string? string?)]
          [poly->coefficients (-> string? string?)]
          ))

(define (get-field-table bit_width field_generator_poly)
  (make-hash))

(define (poly->index_coe_pairs poly)
  (let loop ([loop_list (regexp-split #rx"\\+" poly)]
             [result_list '()])
    (if (not (null? loop_list))
        (loop
         (cdr loop_list)
         (cons
          (let ([items (regexp-split #rx"\\x" (string-trim (car loop_list)))])
            (cond
             [(and (= (length items) 1) (string=? (first items) "1"))
              '(0 . 1)]
             [(= (length items) 2)
              (cons
               (if (string=? (second items) "")
                   1
                   (string->number (second items)))
               1)]
             [(= (length items) 3)
              (cons (string->number (second items)) (first items))]
             [else
              (error (format "invalid poly: [~a]" (string-trim (car loop_list))))]))
          result_list))
        (sort (reverse result_list) > #:key car))))

(define (index_coe_pairs->poly pairs)
  (let loop ([loop_pairs (sort pairs > #:key car)]
             [last_result ""]
             [last_operator ""])
    (if (not (null? loop_pairs))
        (loop
         (cdr loop_pairs)
         (format "~a~a~a"
                 last_result
                 last_operator
                 (if (= (caar loop_pairs) 0)
                     (cdar loop_pairs)
                     (format "~ax~a"
                             (if (= (cdar loop_pairs) 1) "" (cdar loop_pairs))
                             (if (= (caar loop_pairs) 1) "" (caar loop_pairs)))))
           "+")
        last_result)))

(define (poly-multiply poly_multiplicand poly_multiplier [is_galios? #f])
  (let ([poly_multiplicand_pairs (poly->index_coe_pairs poly_multiplicand)]
        [poly_multiplier_pairs (poly->index_coe_pairs poly_multiplier)])
    
    (let loop-multiplicand ([loop_poly_multiplicand_pairs poly_multiplicand_pairs]
                            [multiplicand_list '()])
      (if (not (null? loop_poly_multiplicand_pairs))
          (loop-multiplicand
           (cdr loop_poly_multiplicand_pairs)
           (cons
            (index_coe_pairs->poly
             (let loop-multiplier ([loop_poly_multiplier_pairs poly_multiplier_pairs]
                                   [multiplier_list '()])
               (if (not (null? loop_poly_multiplier_pairs))
                   (loop-multiplier
                    (cdr loop_poly_multiplier_pairs)
                    (cons
                     (cons
                      (+ (caar loop_poly_multiplicand_pairs) (caar loop_poly_multiplier_pairs))
                      (* (cdar loop_poly_multiplicand_pairs) (cdar loop_poly_multiplier_pairs)))
                     multiplier_list))
                   (reverse multiplier_list))))
            multiplicand_list))
          (index_coe_pairs->poly
           (let ([combine_hash (make-hash)])
             (let loop ([loop_polys multiplicand_list])
               (when (not (null? loop_polys))
                 (map
                  (lambda (p)
                    (if (hash-has-key? combine_hash (car p))
                        (if is_galios?
                            (let ([coe_bitwised (bitwise-xor (cdr p) (hash-ref combine_hash (car p)))])
                              (if (= coe_bitwised 0)
                                  (hash-remove! combine_hash (car p))
                                  (hash-set! combine_hash (car p) coe_bitwised)))
                            (hash-set! combine_hash (car p) (+ (hash-ref combine_hash (car p)) (cdr p))))
                        (hash-set! combine_hash (car p) (cdr p))))
                  (poly->index_coe_pairs (car loop_polys)))
                 (loop (cdr loop_polys))))
             (hash->list combine_hash)))))))

(define (galios-poly-multiply poly_multiplicand poly_multiplier)
  (poly-multiply poly_multiplicand poly_multiplier #t))

(define (poly-sum poly)
  (let loop ([pairs (poly->index_coe_pairs poly)]
             [sum 0])
    (if (not (null? pairs))
        (loop (cdr pairs) (+ sum (* (cdar pairs) (expt 2 (caar pairs)))))
        sum)))

(define (poly->equal_pair poly)
  (let ([indexes (poly->index_coe_pairs poly)])
    (cons
     (index_coe_pairs->poly (list (car indexes)))
     (index_coe_pairs->poly (cdr indexes)))))

(define (poly-remove_dup poly)
  (index_coe_pairs->poly
   (let loop ([indexes (poly->index_coe_pairs poly)]
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
      (let ([indexes (map (lambda (p) (car p)) (poly->index_coe_pairs poly))])
        (let loop ([loop_index (car indexes)])
          (when (>= loop_index 0)
            (if (member loop_index indexes)
                (printf "1")
                (printf "0"))
            (loop (sub1 loop_index))))))))
