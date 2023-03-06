#lang racket

(provide (contract-out
          [struct PITEM
                  (
                   (x_index natural?)
                   (coe natural?)
                   )]
          [check-pitem? (-> PITEM? PITEM? boolean?)]
          [check-pitem-list? (-> (listof PITEM?) (listof PITEM?) boolean?)]
          [number->binary_poly (-> natural? string?)]
          [poly->items (-> string? (listof PITEM?))]
          [items->poly (-> (listof PITEM?) string?)]
          [binary_poly->binary_string (-> string? string?)]
          [binary_string->binary_poly (-> string? string?)]
          [binary_poly-multiply (-> string? string? string?)]
          [binary_poly-divide (-> string? string? string?)]
          [galios-multiply (-> natural? natural? natural?)]
          [galios-poly-divide-align (-> string? string? string?)]
          [galios-poly-divide (-> string? string? (values string? string?))]
          [galios-poly-multiply (->* (string? string?) () #:rest (listof string?) string?)]
          [galios-poly-add (->* (string?) () #:rest (listof string?) string?)]
          [get-code-generator-poly (-> natural? string?)]
          [get-galios-index->number_map (-> natural? (hash/c string? number?))]
          [poly-sum (-> string? natural?)]
          [poly-remove_dup (-> string? string?)]
          [*bit_width* parameter?]
          [*field_generator_poly* parameter?]
          [*galios_index->number_map* parameter?]
          [*galios_number->index_map* parameter?]
          ))

(struct PITEM
        (
         (x_index #:mutable)
         (coe #:mutable)
         ))

(define (check-pitem? pitem1 pitem2)
  (and
   (= (PITEM-x_index pitem1) (PITEM-x_index pitem2))
   (= (PITEM-coe pitem1) (PITEM-coe pitem2))))

(define (check-pitem-list? p1 p2)
  (if (= (length p1) (length p2))
      (let loop ([p_items1 p1]
                 [p_items2 p2])
        (if (not (null? p_items1))
            (if (check-pitem? (car p1) (car p2))
                (loop (cdr p_items1) (cdr p_items2))
                #f)
            #t))
      #f))

(define *bit_width* (make-parameter #f))
(define *field_generator_poly* (make-parameter #f))
(define *galios_index->number_map* (make-parameter #f))
(define *galios_number->index_map* (make-parameter #f))

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

(define (poly->items poly)
  (let loop ([loop_list (regexp-split #rx"\\+" poly)]
             [result_list '()])
    (if (not (null? loop_list))
        (loop
         (cdr loop_list)
         (cons
          (let ([items (regexp-split #rx"\\x" (string-trim (car loop_list)))])
            (cond
             [(= (length items) 1)
              (PITEM 0 (string->number (first items)))]
             [(= (length items) 2)
              (PITEM
               (if (string=? (second items) "")
                   1
                   (string->number (second items)))
               (if (string=? (first items) "")
                   1
                   (string->number (first items))))]
             [(= (length items) 3)
              (PITEM (string->number (second items)) (string->number (first items)))]
             [else
              (error (format "invalid poly: [~a]" (string-trim (car loop_list))))]))
          result_list))
        (sort (reverse result_list) > #:key PITEM-x_index))))

(define (items->poly items)
  (let loop ([loop_items (sort items > #:key PITEM-x_index)]
             [last_result ""]
             [last_operator ""])
    (if (not (null? loop_items))
        (loop
         (cdr loop_items)
         (format "~a~a~a"
                 last_result
                 last_operator
                 (if (= (PITEM-x_index (car loop_items)) 0)
                     (PITEM-coe (car loop_items))
                     (format "~ax~a"
                             (if (= (PITEM-coe (car loop_items)) 1) "" (PITEM-coe (car loop_items)))
                             (if (= (PITEM-x_index (car loop_items)) 1) "" (PITEM-x_index (car loop_items))))))
           "+")
        last_result)))

(define (binary_poly->binary_string poly)
  (with-output-to-string
    (lambda ()
      (let ([indexes (map (lambda (p) (PITEM-x_index p)) (poly->items poly))])
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

(define (galios-poly-add poly1 . rst)
  (items->poly
   (let ([combine_hash (make-hash)])
     (let loop-poly ([loop_polys `(,poly1 ,@rst)])
       (when (not (null? loop_polys))
         (let loop-item ([items (poly->items (car loop_polys))])
           (when (not (null? items))
             (let ([item (car items)])
               (if (hash-has-key? combine_hash (PITEM-x_index item))
                (let ([coe_bitwised (bitwise-xor (PITEM-coe item) (hash-ref combine_hash (PITEM-x_index item)))])
                  (if (= coe_bitwised 0)
                      (hash-remove! combine_hash (PITEM-x_index item))
                      (hash-set! combine_hash (PITEM-x_index item) coe_bitwised)))
                (hash-set! combine_hash (PITEM-x_index item) (PITEM-coe item))))
             (loop-item (cdr items))))
         (loop-poly (cdr loop_polys))))
     (map
      (lambda (p)
        (PITEM (car p) (cdr p)))
      (hash->list combine_hash)))))

(define (poly-multiply-basic poly1 poly2 add_op multiply_op)
  (let ([poly_multiplicand_pairs (poly->items poly1)]
        [poly_multiplier_pairs (poly->items poly2)])
                                
    (let loop-multiplicand ([loop_poly_multiplicand_pairs poly_multiplicand_pairs]
                            [multiplicand_list '()])
      (if (not (null? loop_poly_multiplicand_pairs))
          (loop-multiplicand
           (cdr loop_poly_multiplicand_pairs)
           (cons
            (items->poly
             (let loop-multiplier ([loop_poly_multiplier_pairs poly_multiplier_pairs]
                                   [multiplier_list '()])
               (if (not (null? loop_poly_multiplier_pairs))
                   (loop-multiplier
                    (cdr loop_poly_multiplier_pairs)
                    (cons
                     (PITEM
                      (add_op (PITEM-x_index (car loop_poly_multiplicand_pairs)) (PITEM-x_index (car loop_poly_multiplier_pairs)))
                      (multiply_op (PITEM-coe (car loop_poly_multiplicand_pairs)) (PITEM-coe (car loop_poly_multiplier_pairs))))
                     multiplier_list))
                   (reverse multiplier_list))))
            multiplicand_list))
          (apply galios-poly-add multiplicand_list)))))

(define (binary_poly-multiply poly1 poly2)
  (poly-multiply-basic poly1 poly2 + *))

(define (binary_poly-divide dividend_poly divisor_poly)
  (let* ([dividend_poly_bits (binary_poly->binary_string dividend_poly)]
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

(define (get-code-generator-poly parity_length)
  (let ([max_index (sub1 parity_length)])
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

(define (get-galios-index->number_map bit_width)
  (let ([poly_index->decimal_hash (make-hash)]
        [poly_index->poly_hash (make-hash)]
        [poly_index_list '()]
        [2^m_1 (sub1 (expt 2 bit_width))]
        [replace_pair 
         (let ([indexes (poly->items (*field_generator_poly*))])
           (cons
            (items->poly (list (car indexes)))
            (items->poly (cdr indexes))))])

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

(define (poly-sum poly)
  (let loop ([pitems (poly->items poly)]
             [sum 0])
    (if (not (null? pitems))
        (loop (cdr pitems) (+ sum (* (PITEM-coe (car pitems)) (expt 2 (PITEM-x_index (car pitems))))))
        sum)))

(define (poly-remove_dup poly)
  (items->poly
   (let loop ([pitems (poly->items poly)]
              [result_list '()])
     (if (not (null? pitems))
         (if (ormap (lambda (pitem) (check-pitem? (car pitems) pitem)) result_list)
             (loop
              (cdr pitems)
              (remove (car pitems) result_list check-pitem?))
             (loop
              (cdr pitems)
              (cons (car pitems) result_list)))
         (reverse result_list)))))

(define (galios-poly-divide-align dividend divisor)
  (let* (
         [divisor_pitems (poly->items divisor)]
         [dividend_pitems (poly->items dividend)]
         [src_index_n (PITEM-x_index (car divisor_pitems))]
         [src_coe_n (PITEM-coe (car divisor_pitems))]
         [src_coe_a #f]
         [src_coe_a_n #f]
         [dst_index_n (PITEM-x_index (car dividend_pitems))]
         [dst_coe_n (PITEM-coe (car dividend_pitems))]
         [dst_coe_a #f]
         [dst_coe_a_n #f]
         [2^m_1 (sub1 (expt 2 (*bit_width*)))]
         )
    (set! src_coe_a (hash-ref (*galios_number->index_map*) src_coe_n))
    
    (set! src_coe_a_n (string->number (substring src_coe_a 1)))

    (set! dst_coe_a (hash-ref (*galios_number->index_map*) dst_coe_n))

    (set! dst_coe_a_n (string->number (substring dst_coe_a 1)))

    (items->poly
     (list
      (PITEM
       (- dst_index_n src_index_n)
       (hash-ref (*galios_index->number_map*)
                 (format "a~a"
                         (modulo
                          (- (+ 2^m_1 dst_coe_a_n) src_coe_a_n)
                          2^m_1))))))))

(define (galios-poly-divide dividend divisor)
  (let ([divisor_index (PITEM-x_index (car (poly->items divisor)))])
    (let loop ([remainder dividend]
               [quotient ""]
               [last_op ""])
      
      (if (not (string=? remainder ""))
          (let* ([remainder_pitems (poly->items remainder)]
                 [remainder_index (PITEM-x_index (car remainder_pitems))]
                 [remainder_coe (PITEM-coe (car remainder_pitems))])

            (if (= remainder_coe 0)
                (loop (items->poly (cdr remainder_pitems)) quotient last_op)
                (if (>= remainder_index divisor_index)
                    (let (
                          [loop_align_factor #f]
                          [loop_divisor_multiply_factor #f]
                          [loop_substract #f]
                          )

                      (set! loop_align_factor (galios-poly-divide-align remainder divisor))

                      (set! loop_divisor_multiply_factor (galios-poly-multiply divisor loop_align_factor))

                      (set! loop_substract (galios-poly-add remainder loop_divisor_multiply_factor))
                      
                      (loop loop_substract (string-append quotient last_op loop_align_factor) "+"))
                    (values quotient remainder))))
          (values quotient remainder)))))
