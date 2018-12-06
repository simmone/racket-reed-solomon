#lang racket

(require "../../lib.rkt")

(provide (contract-out
          [write-report-galois-fields (-> hash? hash? path-string? void?)]
          ))

(require racket/runtime-path)
(define-runtime-path primitive_poly_tip "primitive_poly_tip")

(define (write-report-galois-fields aton_map ntoa_map express_path)
  (let* ([scrbl_dir (build-path express_path "galois-fields")]
         [scrbl_file (build-path scrbl_dir "galois-fields.scrbl")])

    (make-directory* scrbl_dir)

    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Galois Fields}\n\n")
        (printf "@section{a -> n}\n")
        (printf (display-double-list
                 (sort (hash-keys aton_map) <)
                 (map (lambda (a) (hash-ref aton_map a)) (sort (hash-keys aton_map) <))
                 5))
        (printf "@section{n -> a}\n")
        (printf (display-double-list
                 (sort (hash-keys ntoa_map) <)
                 (map (lambda (a) (hash-ref ntoa_map a)) (sort (hash-keys ntoa_map) <))
                 5))
        ))))

