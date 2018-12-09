#lang racket

(require "../../../share/lib.rkt")

(provide (contract-out
          [write-report-message-poly (-> string? path-string? void?)]
          ))

(define (write-report-message-poly message_poly express_path)
  (let* ([scrbl_dir (build-path express_path "message-poly")]
         [scrbl_file (build-path scrbl_dir "message-poly.scrbl")])

    (make-directory* scrbl_dir)

    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Message Poly}\n\n")
        (printf "@section{@bold{~a}}\n" message_poly)
        ))))

