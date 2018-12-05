#lang racket

(require "../../lib.rkt")

(provide (contract-out
          [write-report-generator-poly (-> string? path-string? void?)]
          ))

(define (write-report-generator-poly generator_poly express_path)
  (let* ([scrbl_dir (build-path express_path "generator-poly")]
         [scrbl_file (build-path scrbl_dir "generator-poly.scrbl")])

    (make-directory* scrbl_dir)

    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Generator Poly}\n\n")
        (printf "@section{@bold{~a}}\n" generator_poly)
        ))))

