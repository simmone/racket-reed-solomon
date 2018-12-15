#lang racket

(require "../../share/lib/lib.rkt")

(provide (contract-out
          [express-start (-> void?)]
          ))

(require racket/runtime-path)
(define-runtime-path header_template "header.template")

(define (express-start)
  (when (*express?*)
        (delete-directory/files #:must-exist? #f (*express_path*))
        (make-directory* (*express_path*))

        (copy-file 
         header_template 
         (build-path (*express_path*) "report.scrbl"))))
