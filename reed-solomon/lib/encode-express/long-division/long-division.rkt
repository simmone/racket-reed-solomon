#lang racket

(require "../../lib.rkt")

(provide (contract-out
          [write-report-long-division-start (-> path-string? void?)]
          [write-report-long-division-detail (-> natural? string? natural? string? natural? string? string? string? string? path-string? void?)]
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

(define (write-report-long-division-detail
         count
         loop_message_poly
         step1_aligned_message_x_length
         step2_aligned_generator_a
         step3_get_first_a
         step4_multiply_a
         step5_to_n
         step6_xor_n
         step7_discard_first_zeros_n
         express_path)

  (let* ([scrbl_dir (build-path express_path "long-division")]
         [scrbl_file (build-path scrbl_dir "long-division.scrbl")])

    (make-directory* scrbl_dir)

    (with-output-to-file
        scrbl_file #:exists 'append
      (lambda ()
        (printf "@section{step: ~a}\n" count)
        (printf "@verbatim{message.header.x.degree::~a}\n" step1_aligned_message_x_length)
        (printf "@verbatim{......prepare.generator::~a}\n" step2_aligned_generator_a)
        (printf "@verbatim{.....generator.header.a::~a}\n" step3_get_first_a)
        (printf "@verbatim{......aligend.generator::~a}\n" step4_multiply_a)
        (printf "@verbatim{.........generator.aton::~a}\n" step5_to_n)
        (printf "@verbatim{...........message.poly::~a}\n" loop_message_poly)
        (printf "@verbatim{..message.xor.generator::~a}\n" step6_xor_n)
        (printf "@verbatim{...discard.first.zeroes::~a}\n" step7_discard_first_zeros_n)
        ))))
