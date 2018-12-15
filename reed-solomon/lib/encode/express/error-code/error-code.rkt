#lang racket

(require "../../../share/lib.rkt")

(provide (contract-out
          [express-error-code (-> natural? (listof exact-integer?) void?)]
          ))

(define (express-error-code bit_width raw_list)
  (when (*express?*)
        (let* ([scrbl_dir (build-path (*express_path*) "error-code")]
               [scrbl_file (build-path scrbl_dir "error-code.scrbl")])

          (make-directory* scrbl_dir)

          (with-output-to-file
              scrbl_file
            (lambda ()
              (printf "#lang scribble/base\n\n")
              (printf "@title{Error Code}\n\n")
              (printf "@section{error code list}\n")
              (printf "~a\n" (display-double-list 
                              raw_list
                              (map
                               (lambda (a)
                                 (~r #:base 2 #:min-width bit_width #:pad-string "0" a))
                               raw_list)))
              )))))

