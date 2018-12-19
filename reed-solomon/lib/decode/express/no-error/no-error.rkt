#lang racket

(require "../../../share/lib.rkt")

(provide (contract-out
          [express-no-error (-> void?)]
          ))

(define (express-no-error)
  (when (*express?*)
        (let* ([scrbl_dir (build-path (*express_path*) "no-error")]
               [scrbl_file (build-path scrbl_dir "no-error.scrbl")])

          (with-output-to-file
              (build-path (*express_path*) "report.scrbl") #:exists 'append
              (lambda ()
                (printf "@include-section[\"no-error/no-error.scrbl\"]\n\n")))

          (make-directory* scrbl_dir)

          (with-output-to-file
              scrbl_file
            (lambda ()
              (printf "#lang scribble/base\n\n")
              (printf "@title{No Error}\n\n")
              (printf "no error symbol need to fix.\n")
              )))))

