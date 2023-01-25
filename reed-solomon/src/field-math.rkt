#lang racket

(provide (contract-out
          [number->binary_poly (-> natural? string?)]
          [binary_poly->binary_string (-> string? string?)]
          [binary_string->binary_poly (-> string? string?)]
          [binary_poly-multiply (-> string? string? string?)]
          [binary_poly-divide (-> string? string? string?)]
          [galios-multiply (-> natural? natural? natural?)]
          [galios-poly-multiply (->* (string? string?) () #:rest (listof string?) string?)]
          [get-code-generator-poly (-> natural? string?)]
          [coefficient-list->poly (-> (listof natural?) natural? natural? string?)]
          [get-galios-index->number_map (-> natural? (hash/c string? number?))]
          [poly->index_coe_pairs (-> string? (listof (cons/c natural? natural?)))]
          [index_coe_pairs->poly (-> (listof (cons/c natural? natural?)) string?)]
          [poly-sum (-> string? natural?)]
          [poly-remove_dup (-> string? string?)]
          [*bit_width* parameter?]
          [*field_generator_poly* parameter?]
          [*galios_index->number_map* parameter?]
          ))

(define *bit_width* (make-parameter #f))
(define *field_generator_poly* (make-parameter #f))
(define *galios_index->number_map* (make-parameter #f))

(define (number->binary_poly num)
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

(define (binary_poly->binary_string poly)
  (with-output-to-string
    (lambda ()
      (let ([indexes (map (lambda (p) (car p)) (poly->index_coe_pairs poly))])
        (let loop ([loop_index (car indexes)])
          (when (>= loop_index 0)
            (if (member loop_index indexes)
                (printf "1")
                (printf "0"))
            (loop (sub1 loop_index))))))))

(define (binary_string->binary_poly bin_str)
  (let loop ([bits (string->list bin_str)]
             [index (sub1 (string-length bin_str))]
             [last_op ""]
             [result ""])
    (if (not (null? bits))
      (if (char=? (car bits) #\0)
          (loop (cdr bits) (sub1 index) last_op result)
          (loop (cdr bits) (sub1 index) "+" 
                (cond
                 [(= index 0) (format "~a~a1" result last_op)]
                 [(= index 1) (format "~a~ax" result last_op)]
                 [else
                    (format "~a~ax~a" result last_op index)])))
      result)))
            
(define (binary_poly-multiply poly1 poly2)
  (poly-multiply-basic poly1 poly2 + *))

(define (binary_poly-divide dividend_poly divisor_poly)
  (let* ([dividend_poly_bits (binary_poly->binary_string dividend_poly)]
         [dividend_bits_length (string-length dividend_poly_bits)]
         [divisor_poly_bits (binary_poly->binary_string divisor_poly)]
         [divisor_bits_length (string-length divisor_poly_bits)])

    (let loop ([loop_bits dividend_poly_bits])
      (if (>= (string-length loop_bits) divisor_bits_length)
          (let ([head_loop_bits (substring loop_bits 0 divisor_bits_length)]
                [bitwise_result #f])
            (set! bitwise_result
                  (number->string
                   (bitwise-xor
                    (string->number head_loop_bits 2)
                    (string->number divisor_poly_bits 2))
                   2))
            (loop (string-append bitwise_result (substring loop_bits divisor_bits_length))))
          (binary_string->binary_poly loop_bits)))))

(define (galios-multiply num1 num2)
  (if (or (= num1 0) (= num2 0))
      0
      (string->number
       (binary_poly->binary_string
        (binary_poly-divide
         (binary_poly-multiply
          (number->binary_poly num1)
          (number->binary_poly num2))
         (*field_generator_poly*)))
       2)))

(define (galios-poly-multiply poly1 poly2 . rst)
  (let loop ([polys rst]
             [last_result (poly-multiply-basic poly1 poly2 + galios-multiply)])
    (if (not (null? polys))
        (loop
         (cdr polys)
         (poly-multiply-basic last_result (car polys) + galios-multiply))
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

(define (get-code-generator-poly bit_width)
  (let ([max_index (sub1 (expt 2 (/ bit_width 2)))])
    (apply
     galios-poly-multiply
     (let loop ([index 0]
                [poly_list '()])
       (if (<= index max_index)
           (loop
            (add1 index)
            (cons
             (format "x+~a" (hash-ref (*galios_index->number_map*) (format "a~a" index)))
             poly_list))
           (reverse poly_list))))))

(define (coefficient-list->poly coefficient_list bit_width parity_length)
  (index_coe_pairs->poly
   (poly->index_coe_pairs
    (let ([max_index (- (expt 2 bit_width) 2)])
      (if (> (length coefficient_list) (- (expt 2 bit_width) parity_length))
          (error (format "coefficient_list'length is too large:[~a][~a][~a]" (length coefficient_list) bit_width parity_length))
          (let loop ([coefficients coefficient_list]
                     [loop_index max_index]
                     [last_op ""]
                     [result ""])
            (if (not (null? coefficients))
                (loop
                 (cdr coefficients)
                 (sub1 loop_index)
                 "+"
                 (format "~a~a~ax~a" result last_op (car coefficients) loop_index))
                result)))))))

(define (get-galios-index->number_map bit_width)
  (let ([poly_index->decimal_hash (make-hash)]
        [poly_index->poly_hash (make-hash)]
        [poly_index_list '()]
        [2^m_1 (sub1 (expt 2 bit_width))]
        [replace_pair 
         (let ([indexes (poly->index_coe_pairs (*field_generator_poly*))])
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
