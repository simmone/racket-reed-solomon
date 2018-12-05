#lang racket

(require "../../lib.rkt")

(provide (contract-out
          [write-report-long-division-start (-> path-string? void?)]
          [write-report-long-division-prepare-message (-> natural? string? path-string? void?)]
          ))

(define (write-report-long-division-start express_path)
  (let* ([scrbl_dir (build-path express_path "long-division")]
         [scrbl_file (build-path scrbl_dir "long-division.scrbl")])

    (make-directory* scrbl_dir)

    (with-output-to-file
        scrbl_file
      (lambda ()
        (printf "#lang scribble/base\n\n")
        (printf "@title{Long Division}\n\n")
        ))))

(define (write-report-long-division-prepare-message count loop_message_poly express_path)
  (let* ([scrbl_dir (build-path express_path "long-division")]
         [scrbl_file (build-path scrbl_dir "long-division.scrbl")])

    (make-directory* scrbl_dir)

    (with-output-to-file
        scrbl_file #:exists 'append
      (lambda ()
        (printf "@section{step: ~a}\n" count)
        (printf "@verbatim{~a}\n" loop_message_poly)
        ))))


