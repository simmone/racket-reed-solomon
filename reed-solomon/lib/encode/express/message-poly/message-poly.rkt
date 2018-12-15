#lang racket

(require "../../../share/lib.rkt")

(provide (contract-out
          [express-message-poly (-> string? void?)]
          ))

(define (express-message-poly message_poly)
  (when (*express?*)
        (let* ([scrbl_dir (build-path (*express_path*) "message-poly")]
               [scrbl_file (build-path scrbl_dir "message-poly.scrbl")])

          (make-directory* scrbl_dir)

          (with-output-to-file
              scrbl_file
            (lambda ()
              (printf "#lang scribble/base\n\n")
              (printf "@title{Message Poly}\n\n")
              (printf "@section{@bold{~a}}\n" message_poly)
              )))))

