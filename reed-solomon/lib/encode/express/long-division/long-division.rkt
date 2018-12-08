#lang racket

(require "../../../lib.rkt")

(provide (contract-out
          [write-report-long-division-start (-> path-string? void?)]
          [write-report-long-division-detail (-> natural? string? string? natural? natural? string? natural? string? string? string? string? natural? path-string? void?)]
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
         generator_poly
         loop_message_n
         step1_message_x
         step2_message_n
         step3_aligned_generator_by_x
         step4_message_n_to_a
         step5_multiply_a
         step6_to_n
         step7_xor_n
         step8_discard_first_zeros_n
         step9_zeros_count
         express_path)

  (let* ([scrbl_dir (build-path express_path "long-division")]
         [scrbl_file (build-path scrbl_dir "long-division.scrbl")])

    (make-directory* scrbl_dir)

    (with-output-to-file
        scrbl_file #:exists 'append
      (lambda ()
        (printf "@section{step: ~a}\n" count)
        (printf "@verbatim{............................message::~a}\n" loop_message_n)
        (printf "@verbatim{..........................generator::~a}\n" generator_poly)
        (printf "@verbatim{............message.header.x.degree::~a}\n" step1_message_x)
        (printf "@verbatim{align.generator.by.message.header.x::~a}\n" step3_aligned_generator_by_x)
        (printf "@verbatim{...................message.header.n::~a}\n" step2_message_n)
        (printf "@verbatim{..............message.header.n.to.a::~a}\n" step4_message_n_to_a)
        (printf "@verbatim{align.generator.by.message.header.a::~a}\n" step5_multiply_a)
        (printf "@verbatim{.....................generator.to.n::~a}\n" step6_to_n)
        (printf "@verbatim{............................message::~a}\n" loop_message_n)
        (printf "@verbatim{..............message.xor.generator::~a}\n" step7_xor_n)
        (printf "@verbatim{.................prefix.zeros.count::~a}\n" step9_zeros_count)
        (printf "@verbatim{...............discard.first.zeroes::~a}\n" step8_discard_first_zeros_n)
        ))))
