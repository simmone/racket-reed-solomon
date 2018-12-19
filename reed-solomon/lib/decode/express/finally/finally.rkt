#lang racket

(require "../../../share/lib.rkt")

(provide (contract-out
          [express-finally (-> (listof exact-integer?) natural? void?)]
          ))

(define (express-finally raw_list bit_width)
  (when (*express?*)
        (let* ([scrbl_dir (build-path (*express_path*) "finally")]
               [scrbl_file (build-path scrbl_dir "finally.scrbl")])

          (with-output-to-file
              (build-path (*express_path*) "report.scrbl") #:exists 'append
              (lambda ()
                (printf "@include-section[\"finally/finally.scrbl\"]\n\n")))

          (make-directory* scrbl_dir)

          (with-output-to-file
              scrbl_file
            (lambda ()
              (printf "#lang scribble/base\n\n")
              (printf "@title{Corrected Values}\n\n")
              (printf "finally, recover data by error data xor error correction.\n")
              (printf "@section{Final Data}\n")
              (printf (display-double-list
                       raw_list
                       (map (lambda (num) (~r #:base 2 #:min-width bit_width #:pad-string "0" num)) raw_list)
                       (add1 bit_width)
                       10))
              )))))

