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
        
        (set! out (open-output-file scrbl_file #:exists 'replace))

        (fprintf out "#lang scribble/base\n\n")

        (fprintf out "@title{Forney Algorithm}\n\n")))

    (let ([derivative_lam (derivative-lam lam_poly)])
      (map
       (lambda (error_index)
         (let ([ome_a #f]
               [delam_a #f]
               [cal_a #f]
               [positive_a #f]
               [modulo_a #f]
               [result_n #f])
           
           (fprintf out "@section{error_index:~a}\n" error_index)
           
           (fprintf out "@verbatim{omega poly:~a}\n" ome_poly)

           (set! ome_a (poly-gf-n-sub-x->a ome_poly (- (*2^m_1*) error_index)))
           
           (fprintf out "@verbatim{omega poly replace x width a^(-~a) = ~a(alpha)}\n" error_index ome_a)

           (fprintf out "@verbatim{lambda poly:~a}\n" lam_poly)
           
           (fprintf out "@verbatim{derivative lambda poly:~a}\n" derivative_lam)

           (set! delam_a (poly-gf-n-sub-x->a derivative_lam (- (*2^m_1*) error_index)))

           (fprintf out "@verbatim{derivative lambda poly replace x width a^(-~a) = ~a(alpha)}\n" error_index delam_a)
           
           (set! cal_a (+ error_index (- ome_a delam_a)))

           (fprintf out "@verbatim{a^~a*(transformed_omega_poly/transformed_derivative_lamda_poly)}\n" error_index)

           (fprintf out "@verbatim{=(+ ~a (- ~a ~a) ) = ~a}\n" error_index ome_a delam_a cal_a)
           
           (set! positive_a (+ cal_a (*2^m_1*)))

           (fprintf out "@verbatim{turn perhaps negative ~a to ~a by add ~a}\n" cal_a positive_a (*2^m_1*))

           (set! modulo_a (modulo positive_a (*2^m_1*)))

           (set! result_n (hash-ref (*gf_aton_map*) modulo_a))

           (fprintf out "@verbatim{turn alpha (~a % ~a) = ~a to number = ~a}\n" positive_a (*2^m_1*) modulo_a result_n)

           (cons error_index result_n)))
       err_places))))
