#lang racket

(require "../share/gf.rkt")
(require "../share/lib.rkt")

(provide (contract-out
          [get-syndromes (->* ((listof exact-integer?) natural?) (boolean?) (listof exact-integer?))]
          ))

(define (get-syndromes raw_list error_code_length [need-express? #f])
  (let ([out (open-output-nowhere)])
    (when (and need-express? (*express?*))
      (let* ([scrbl_dir (build-path (*express_path*) "syndrome")]
             [scrbl_file (build-path scrbl_dir "syndrome.scrbl")])

        (with-output-to-file
            (build-path (*express_path*) "report.scrbl") #:exists 'append
            (lambda ()
              (printf "@include-section[\"syndrome/syndrome.scrbl\"]\n\n")))

        (make-directory* scrbl_dir)
        
        (set! out (open-output-file scrbl_file #:exists 'replace))

        (fprintf out "#lang scribble/base\n\n")

        (fprintf out "@title{Get Syndromes}\n\n")))
    
    (let loop ([loop_a_count 0]
               [result_list '()])
      (if (< loop_a_count error_code_length)
          (begin
            (fprintf out "@section{S~a}\n" loop_a_count)
            (loop
             (add1 loop_a_count)
             (cons
              (let xor-loop ([loop_n_list raw_list]
                             [last_result_n 0])
                (if (> (length loop_n_list) 1)
                    (let ([ri (car loop_n_list)]
                          [gf+ #f]
                          [ntoa #f]
                          [a+ #f]
                          [aton #f])

                      (set! gf+ (bitwise-xor last_result_n (car loop_n_list)))

                      (set! ntoa (hash-ref (*gf_ntoa_map*) gf+ 0))

                      (if (= gf+ 0)
                          (begin
                            (set! a+ 0)
                            (set! aton 0))
                          (begin
                            (set! a+ (+ loop_a_count ntoa))

                            (set! aton (hash-ref (*gf_aton_map*) (modulo a+ (*2^m_1*))))))
                      
                      (fprintf out "@verbatim{( ~a + ~a ) x 2^~a = ~a}\n" last_result_n (car loop_n_list) loop_a_count aton)

                      (xor-loop (cdr loop_n_list) aton))
                    (let ([result (bitwise-xor last_result_n (car loop_n_list))])
                      (fprintf out "@verbatim{ ~a + ~a = ~a}\n" last_result_n (car loop_n_list) result)
                      result)))
              result_list)))
            result_list))))
  
