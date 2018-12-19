#lang racket

(require "../../../share/lib.rkt")

(provide (contract-out
          [express-primitive-poly (-> void?)]
          ))

(require racket/runtime-path)
(define-runtime-path primitive_poly_tip "primitive_poly_tip")

(define (express-primitive-poly)
  (when (*express?*)
        (let* ([scrbl_dir (build-path (*express_path*) "primitive-poly")]
               [scrbl_file (build-path scrbl_dir "primitive-poly.scrbl")])

          (with-output-to-file
              (build-path (*express_path*) "report.scrbl") #:exists 'append
              (lambda ()
                (printf "@include-section[\"primitive-poly/primitive-poly.scrbl\"]\n\n")))

          (make-directory* scrbl_dir)

          (with-output-to-file
              scrbl_file
            (lambda ()
              (printf "#lang scribble/base\n\n")
              (printf "@title{Appendix. Primitive Poly}\n\n")
              (printf "@section[#:tag \"PrimitivePoly\"]{Primitive Poly Refrence Table}\n")
              (printf "@verbatim{\n~a\n}\n" (file->string primitive_poly_tip))
              )))))

