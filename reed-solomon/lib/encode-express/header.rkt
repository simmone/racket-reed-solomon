#lang racket

(provide (contract-out
          [write-report-header (-> path-string? void?)]
          ))

(require racket/runtime-path)
(define-runtime-path header_template "header.template")

(define (write-report-header num_list patrity_length bit_width primitive_poly express_path)
  (copy-file 
   header_template 
   (build-path express_path "report.scrbl")))
