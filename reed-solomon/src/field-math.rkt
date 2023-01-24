#lang racket

(provide (contract-out
          [number->poly (-> natural? string?)]
          [binary_poly-multiply (-> string? string? string?)]
          [get-galios-a->n_map (-> natural? string? hash?)]
          [poly->index_coe_pairs (-> string? (listof (cons/c natural? natural?)))]
          [index_coe_pairs->poly (-> (listof (cons/c natural? natural?)) string?)]
          [galios-poly-multiply (->* (string? string?) () #:rest (listof string?) string?)]
          [poly-sum (-> string? natural?)]
          [poly-remove_dup (-> string? string?)]
          [poly->coefficients (-> string? string?)]
          ))

(define (number->poly num)
  (let ([bit_list (string->list (number->string num 2))])
    (let loop ([bit_chars bit_list] 
               [index (sub1 (length bit_list))]
               [last_op ""]
               [result ""])
      (if (not (null? bit_chars))
          (if (char=? (car bit_chars) #\0)
              (loop (cdr bit_chars) (sub1 index) last_op result)
              (loop
               (cdr bit_chars)
               (sub1 index)
               "+"
               (format "~a~a~a" result last_op
                       (cond
                        [(= index 1)
                         "x"]
                        [(= index 0)
                         "1"]
                        [else
                         (format "x~a" index)]))))
          result))))

(define (binary_poly-multiply poly1 poly2)
  (poly-multiply-basic poly1 poly2 + *))

(define (galios-poly-multiply poly1 poly2 . rst)
  (let loop ([polys rst]
             [last_result (poly-multiply-basic poly1 poly2 + *)])
    (if (not (null? polys))
        (loop
         (cdr polys)
         (poly-multiply-basic last_result (car polys) + *))
        last_result)))

(define (poly-multiply-basic poly1 poly2 add_op multiply_op)
  (let ([poly_multiplicand_pairs (poly->index_coe_pairs poly1)]
        [poly_multiplier_pairs (poly->index_coe_pairs poly2)])
                                
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
                      (add_op (caar loop_poly_multiplicand_pairs) (caar loop_poly_multiplier_pairs))
                      (multiply_op (cdar loop_poly_multiplicand_pairs) (cdar loop_poly_multiplier_pairs)))
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
                        (let ([coe_bitwised (bitwise-xor (cdr p) (hash-ref combine_hash (car p)))])
                          (if (= coe_bitwised 0)
                              (hash-remove! combine_hash (car p))
                              (hash-set! combine_hash (car p) coe_bitwised)))
                        (hash-set! combine_hash (car p) (cdr p))))
                  (poly->index_coe_pairs (car loop_polys)))
                 (loop (cdr loop_polys))))
             (hash->list combine_hash)))))))

(define (get-galios-a->n_map bit_width field_generator_poly)
  (let ([poly_index->decimal_hash (make-hash)]
        [poly_index->poly_hash (make-hash)]
        [poly_index_list '()]
        [2^m_1 (sub1 (expt 2 bit_width))]
        [replace_pair 
         (let ([indexes (poly->index_coe_pairs field_generator_poly)])
           (cons
            (index_coe_pairs->poly (list (car indexes)))
            (index_coe_pairs->poly (cdr indexes))))])

    (hash-set! poly_index->poly_hash "0" "0")
    (hash-set! poly_index->decimal_hash "0" 0)
    (set! poly_index_list `(,@poly_index_list "0"))

    (hash-set! poly_index->poly_hash "a0" "1")
    (hash-set! poly_index->decimal_hash "a0" 1)
    (set! poly_index_list `(,@poly_index_list "a0"))
    
    (let loop ([index 1]
               [last_val "1"])
      (when (< index 2^m_1)
        (let ([a_index (format "a~a" index)]
              [step1_last_val*2^1_poly #f]
              [step2_replaced_poly #f]
              [step3_remove_duplicates #f])

          (set! poly_index_list `(,@poly_index_list ,a_index))

          (set! step1_last_val*2^1_poly (galios-poly-multiply last_val "x"))

          (set! step2_replaced_poly (regexp-replace* (regexp (car replace_pair)) step1_last_val*2^1_poly (cdr replace_pair)))

          (set! step3_remove_duplicates (poly-remove_dup step2_replaced_poly))

          (hash-set! poly_index->poly_hash a_index step3_remove_duplicates)
          (hash-set! poly_index->decimal_hash a_index (poly-sum step3_remove_duplicates))

          (loop (add1 index) step3_remove_duplicates))))
    
    poly_index->decimal_hash))

(define (poly->index_coe_pairs poly)
  (let loop ([loop_list (regexp-split #rx"\\+" poly)]
             [result_list '()])
    (if (not (null? loop_list))
        (loop
         (cdr loop_list)
         (cons
          (let ([items (regexp-split #rx"\\x" (string-trim (car loop_list)))])
            (cond
             [(= (length items) 1)
              (cons 0 (string->number (first items)))]
             [(= (length items) 2)
              (cons
               (if (string=? (second items) "")
                   1
                   (string->number (second items)))
               (if (string=? (first items) "")
                   1
                   (string->number (first items))))]
             [(= (length items) 3)
              (cons (string->number (second items)) (string->number (first items)))]
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


(define (poly-sum poly)
  (let loop ([pairs (poly->index_coe_pairs poly)]
             [sum 0])
    (if (not (null? pairs))
        (loop (cdr pairs) (+ sum (* (cdar pairs) (expt 2 (caar pairs)))))
        sum)))

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
