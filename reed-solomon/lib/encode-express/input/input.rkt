#lang racket

(require "../../lib.rkt")

(provide (contract-out
          [write-report-input (-> (listof exact-integer?) natural? natural? natural? path-string? void?)]
          ))

(require racket/runtime-path)
(define-runtime-path mode_tip "mode_tip")

(define (write-report-input raw_list patrity_length bit_width primitive_poly express_path)
  (let* ([scrbl_dir (build-path express_path "input")]
         [scrbl_file (build-path scrbl_dir "input.scrbl")])

    (make-directory* scrbl_dir)

    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Input}\n\n")
        (printf "collect input factors.\n")
        (printf "@section{Bit Width:@bold{~a}}\n" bit_width)
        (printf "@section{Raw Data}\n")
        (printf (display-double-list
                 raw_list
                 (map (lambda (num) (~r #:base 2 #:min-width bit_width #:pad-string "0" num)) raw_list)
                 (add1 bit_width)))
        ))))

