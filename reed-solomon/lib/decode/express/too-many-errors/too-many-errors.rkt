#lang racket

(require "../../../share/lib.rkt")

(provide (contract-out
          [express-too-many-errors (-> void?)]
          ))

(define (express-too-many-errors)
  (when (*express?*)
        (let* ([scrbl_dir (build-path (*express_path*) "too-many-errors")]
               [scrbl_file (build-path scrbl_dir "too-many-errors.scrbl")])

          (with-output-to-file
              (build-path (*express_path*) "report.scrbl") #:exists 'append
              (lambda ()
                (printf "@include-section[\"too-many-errors/too-many-errors.scrbl\"]\n\n")))

          (make-directory* scrbl_dir)

          (with-output-to-file
              scrbl_file
            (lambda ()
              (printf "#lang scribble/base\n\n")
              (printf "@title{Too Many Errors}\n\n")
              (printf "too many errors occurs, can't be fixed, stop.\n")
              )))))

