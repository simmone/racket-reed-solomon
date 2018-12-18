#lang racket

(require "../../../share/lib.rkt")

(provide (contract-out
          [express-euclidean-decode (-> void?)]
          ))

(require racket/runtime-path)
(define-runtime-path euclidean_decode_tip "euclidean_decode_tip")

(define (express-euclidean-decode)
  (when (*express?*)
        (let* ([scrbl_dir (build-path (*express_path*) "euclidean-decode")]
               [scrbl_file (build-path scrbl_dir "euclidean-decode.scrbl")])

          (make-directory* scrbl_dir)

          (with-output-to-file
              scrbl_file
            (lambda ()
              (printf "#lang scribble/base\n\n")
              (printf "@title{Appendix. Euclidean Decode Procedure}\n\n")
              (printf "@section[#:tag \"EuclideanDecode\"]{Get Error Locator Poly}\n")
              (printf "@verbatim{\n~a\n}\n" (file->string euclidean_decode_tip))
              )))))

