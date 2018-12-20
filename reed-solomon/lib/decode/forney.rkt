#lang racket

(require "../share/lib.rkt")
(require "../share/gf.rkt")
(require "../share/poly.rkt")
(require "../share/euclidean.rkt")

(provide (contract-out
          [derivative-lam (-> string? string?)]
          [forney (->* (string? string? (listof exact-integer?)) (boolean?) (listof pair?))]
          ))

(define (derivative-lam lam_poly)
  (let-values ([(quotient remainder)
                (euc-divide
                 (poly-n->string
                  (filter
                   (lambda (item)
                     (odd? (cdr item)))
                   (string-n->poly lam_poly)))
                 "x")])
    quotient))

(define (forney lam_poly ome_poly err_places [need-express? #f])
  (let ([out (open-output-nowhere)])
    (when (and need-express? (*express?*))
      (let* ([scrbl_dir (build-path (*express_path*) "forney")]
             [scrbl_file (build-path scrbl_dir "forney.scrbl")])

        (with-output-to-file
            (build-path (*express_path*) "report.scrbl") #:exists 'append
            (lambda ()
              (printf "@include-section[\"forney/forney.scrbl\"]\n\n")))

        (make-directory* scrbl_dir)
        
        (set! out (open-output-file scrbl_file #:exists 'replace))))
    
    (map
     (lambda (error_index)
       (let ([derivative_lam (derivative-lam lam_poly)]
             [ome_a #f]
             [delam_a #f])
         
         (printf "error_index:~a\n" error_index)
         
         (printf "ome_poly:~a\n" ome_poly)

         (set! ome_a (poly-gf-n-sub-x->a ome_poly (- (*2^m_1*) error_index)))
         
         (printf "ome_a:~a\n" ome_a)

         (printf "delam_poly:~a\n" derivative_lam)

         (set! delam_a (poly-gf-n-sub-x->a derivative_lam (- (*2^m_1*) error_index)))

         (printf "delam_a:~a\n" delam_a)

         (cons
          error_index
          (hash-ref (*gf_aton_map*) (modulo (+ (+ (- ome_a delam_a) error_index) (*2^m_1*)) (*2^m_1*))))))
     err_places)))
