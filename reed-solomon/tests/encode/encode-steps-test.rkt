#lang racket

(require racket/pretty)

(require rackunit/text-ui)

(require rackunit "../../lib/encode/encode.rkt")

(define test-encode
  (test-suite 
   "test-encode"

   (test-case
    "test-encode"
    
;;    (check-equal?
;;     (rs-encode '(32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17) 10)
;;     (list 196  35  39  119  235  215  231  226  93  23))
    
    (let ([input_data_list '(32 91 11 120 209 114 220 77 67 64 236 17 236 17 236 17)]
          [parity_length 10])

      (printf "start enode\n\n")
      `
      (printf "input data list:\n\n")
      
      (let loop ([loop_data_list input_data_list]
                 [count 1])
        (when (not (null? loop_data_list))
          (if (<= count 10)
              (begin
                (printf "[~a] " (car loop_data_list))
                (loop (cdr loop_data_list) (add1 count)))
              (begin
                (printf "\n[~a] " (car loop_data_list))
                (loop (cdr loop_data_list) 2)))))
      
      (printf "\n")
    )
    )
   ))

(run-tests test-encode)
