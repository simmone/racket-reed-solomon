#lang racket

(require "../share/lib.rkt")
(require "../share/gf.rkt")
(require "../share/poly.rkt")
(require "../share/euclidean.rkt")

(provide (contract-out
          [error-locator-poly (->* (string? natural?) (boolean?) (values (or/c #f string?) (or/c #f string?)))]
          ))

(define (error-locator-poly syndrome_poly_n error_length [need-express? #t])
  (let ([out (open-output-nowhere)])
    (when (and need-express? (*express?*))
      (let* ([scrbl_dir (build-path (*express_path*) "error-locator")]
             [scrbl_file (build-path scrbl_dir "error-locator.scrbl")])

        (with-output-to-file
            (build-path (*express_path*) "report.scrbl") #:exists 'append
            (lambda ()
              (printf "@include-section[\"error-locator/error-locator.scrbl\"]\n\n")))

        (make-directory* scrbl_dir)
        
        (set! out (open-output-file scrbl_file #:exists 'replace))))
    
    (fprintf out "#lang scribble/base\n\n")

    (fprintf out "@title{Error Locator Poly}\n\n")

    (fprintf out "algorithm is here: @seclink{EuclideanDecode}\n")

    (let loop ([i 1]
               [r-2 (format "x~a" (* 2 error_length))]
               [r-1 syndrome_poly_n]
               [t-2 "0"]
               [t-1 "1"])

      (fprintf out "@section{i = ~a}\n" i)

      (let ([q #f]
            [r #f]
            [t #f]
            [r_degree #f])
        
        (fprintf out "@verbatim{r~a:~a, " (- i 2) r-2)
        (fprintf out "r~a:~a, " (- i 1) r-1)
        (fprintf out "t~a:~a, " (- i 2) t-2)
        (fprintf out "t~a:~a}\n" (- i 1) t-1)

        (let-values ([(quotient remainder) (euc-divide r-2 r-1)])
          (set! q quotient)
          (set! r remainder))

        (fprintf out "@verbatim{q~a:~a}\n" i q)
        (fprintf out "@verbatim{r~a:~a}\n" i r)
        
        (set! t (poly-n-add t-2 (poly-gf-n-multiply q t-1)))

        (fprintf out "@verbatim{t~a:~a}\n" i t)
        
        (set! r_degree (cdr (poly-n-car r)))
        
        (if (>= r_degree error_length)
            (loop
             (add1 i)
             r-1
             r
             t-1
             t)
            (let ([t0 (number->string (car (poly-n-tail t)))])

              (fprintf out "@section{r's degree(~a) < error length(~a), end loop}\n" r_degree error_length)

              (fprintf out "@verbatim{t(0):~a}\n" t0)

              (if (string=? t0 "0")
                  (begin
                    (fprintf out "@verbatim{too much errors occurs(> ~a), can't be fixed. }\n" error_length)
                    (values #f #f))
                  (let-values ([(ome_quotient ome_remainder) (euc-divide r t0)]
                               [(lam_quotient lam_remainder) (euc-divide t t0)])
                    (fprintf out "@verbatim{ome = (/ ~a ~a) = ~a}\n" r t0 ome_quotient)
                    (fprintf out "@verbatim{lam = (/ ~a ~a) = ~a}\n" t t0 lam_quotient)
                    (values ome_quotient lam_quotient)))))))))
