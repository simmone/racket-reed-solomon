#lang racket

(require "../../../src/lib/field-math.rkt")

;; with selected bit_width and field_generator_poly, generate Galois field elements hash table.

(define (get-field-elements-table bit_width field_generator_poly)
  (printf "Field-Math: get-field-elements-table\n\n")
  
  (printf "bitwidth: ~a\n\n" bit_width)
  
  (printf "field_generator_poly: ~a\n\n" field_generator_poly)
  
  (let ([poly_index->decimal_hash (make-hash)]
        [poly_index->poly_hash (make-hash)]
        [poly_index_list '()]
        [2^m_1 (sub1 (expt 2 bit_width))]
        [replace_pair (poly_a->equal_pair field_generator_poly)])

    (hash-set! poly_index->poly_hash "0" "0")
    (hash-set! poly_index->decimal_hash "0" "0")
    (set! poly_index_list `(,@poly_index_list "0"))

    (hash-set! poly_index->poly_hash "a0" "1")
    (hash-set! poly_index->decimal_hash "a0" "1")
    (set! poly_index_list `(,@poly_index_list "a0"))
    
    (printf "calculating each index's field element\n\n")
    (let loop ([index 1]
               [last_val "1"])
      (when (< index 2^m_1)
        (let ([a_index (format "a~a" index)]
              [step1_last_val*2^1_poly #f]
              [step2_replaced_poly #f]
              [step3_remove_duplicates #f])

          (printf "index: ~a\n" a_index)

          (set! poly_index_list `(,@poly_index_list ,a_index))

          (printf "  step1: (poly_a-multiply-n last_val 1):\n")
          (set! step1_last_val*2^1_poly (poly_a-multiply-n last_val 1))
          (printf "    (poly_a-multiply-n ~a 1) = ~a\n" last_val step1_last_val*2^1_poly)

          (printf "  step2: replace poly item by field_generator_poly:~a\n" replace_pair)
          (set! step2_replaced_poly (regexp-replace* (regexp (car replace_pair)) step1_last_val*2^1_poly (cdr replace_pair)))
          (printf "    (regxp-replace* #rx\"~a\" poly ~a) = ~a\n" (car replace_pair) (cdr replace_pair) step2_replaced_poly)

          (printf "  step3: remove all the duplicates poly items:\n")
          (set! step3_remove_duplicates (poly_a-remove_dup step2_replaced_poly))
          (printf "    (poly_a-remove_dup ~a) = ~a\n" step2_replaced_poly step3_remove_duplicates)

          (hash-set! poly_index->poly_hash a_index step3_remove_duplicates)
          (hash-set! poly_index->decimal_hash a_index (poly_a->sum step3_remove_duplicates))

          (loop (add1 index) step3_remove_duplicates))))

    (printf "\nfield_elements_table:\n[~a][~a][~a]\n"
            (~a #:min-width 10 #:align 'left #:right-pad-string " " "index form")
            (~a #:min-width 40 #:align 'left #:right-pad-string " " "polynomial form")
            (~a #:min-width 20 #:align 'left #:right-pad-string " " "decimal form"))

    (let loop ([loop_list poly_index_list])
      (when (not (null? loop_list))
        (printf "[~a][~a][~a]\n"
                (~a #:min-width 10 #:align 'left #:right-pad-string " " (car loop_list))
                (~a #:min-width 40 #:align 'left #:right-pad-string " " (hash-ref poly_index->poly_hash (car loop_list)))
                (~a #:min-width 20 #:align 'left #:right-pad-string " " (hash-ref poly_index->decimal_hash (car loop_list))))
      
        (loop (cdr loop_list))))

    (printf "\n\n")

    )
  )

(get-field-elements-table 4 "a4+a+1")
