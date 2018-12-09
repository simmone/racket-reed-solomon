#lang racket

(require "../../../share/lib.rkt")

(provide (contract-out
          [write-report-primitive-poly (-> path-string? void?)]
          ))

(require racket/runtime-path)
(define-runtime-path primitive_poly_tip "primitive_poly_tip")

(define (write-report-primitive-poly express_path)
  (let* ([scrbl_dir (build-path express_path "primitive-poly")]
         [scrbl_file (build-path scrbl_dir "primitive-poly.scrbl")])

    (make-directory* scrbl_dir)

    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Appendix. Primitive Poly}\n\n")
        (printf "@section[#:tag \"PrimitivePoly\"]{Primitive Poly Refrence Table}\n")
        (printf "@verbatim{\n~a\n}\n" (file->string primitive_poly_tip))
        ))))

