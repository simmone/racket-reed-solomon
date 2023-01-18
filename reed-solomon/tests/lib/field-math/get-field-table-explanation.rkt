#lang racket

;; with selected bit_width and field_generator_poly, generate Galois field elements hash table.

(define (get-field-elements-table bit_width field_generator_poly)
  (printf "Field-Math: get-field-elements-table\n\n")
  
  (printf "bitwidth: ~a\n\n" bit_width)
  
  (printf "field_generator_poly: ~a\n\n" field_generator_poly)
  
  (let ([poly_index->decimal_hash (make-hash)]
        [poly_index->poly_hash (make-hash)]
        [2^m_1 (sub1 (expt 2 bit_width))])

    (hash-set! poly_index->decimal_hash "0" "0")
    (hash-set! poly_index->poly_hash "0" "0")
    
    (printf "calculating each index's field element\n\n")
    (let loop ([index 0])
      (when (< index 2^m_1)
        (printf "index: ~a\n" index)
        (loop (add1 index))))

    (printf "[~a][~a][~a]\n"
            (~a #:min-width 10 #:align 'left #:right-pad-string " " "index form")
            (~a #:min-width 20 #:align 'left #:right-pad-string " " "polynomial form")
            (~a #:min-width 20 #:align 'left #:right-pad-string " " "decimal form"))
    (let loop ([loop_list (sort (hash-keys poly_index->decimal_hash) string<?)])
      (when (not (null? loop_list))
        (printf "[~a][~a][~a]\n"
                (~a #:min-width 10 #:align 'left #:right-pad-string " " (car loop_list))
                (~a #:min-width 20 #:align 'left #:right-pad-string " " (hash-ref poly_index->poly_hash (car loop_list)))
                (~a #:min-width 20 #:align 'left #:right-pad-string " " (hash-ref poly_index->decimal_hash (car loop_list))))
      
        (loop (cdr loop_list))))

    (printf "\n\n")

    )
  )

(get-field-elements-table 4 "a4+a+1")
