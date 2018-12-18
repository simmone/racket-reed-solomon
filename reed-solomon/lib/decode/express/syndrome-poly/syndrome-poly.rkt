#lang racket

(require "../../../share/lib.rkt")

(provide (contract-out
          [express-syndrome-poly (-> string? void?)]
          ))

(define (express-syndrome-poly syndrome_poly)
  (when (*express?*)
        (let* ([scrbl_dir (build-path (*express_path*) "syndrome-poly")]
               [scrbl_file (build-path scrbl_dir "syndrome-poly.scrbl")])

          (make-directory* scrbl_dir)

          (with-output-to-file
              scrbl_file
            (lambda ()
              (printf "#lang scribble/base\n\n")
              (printf "@title{Syndrome Poly}\n\n")
              (printf "@verbatim{@bold{~a}}\n" syndrome_poly)
              )))))

