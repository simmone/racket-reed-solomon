#lang racket

(require "../../../share/lib.rkt")

(provide (contract-out
          [express-input (-> (listof exact-integer?) natural? natural? natural? void?)]
          ))

(define (express-input raw_list error_code_length bit_width primitive_poly_value)
  (when (*express?*)
        (let* ([scrbl_dir (build-path (*express_path*) "input")]
               [scrbl_file (build-path scrbl_dir "input.scrbl")])

          (with-output-to-file
              (build-path (*express_path*) "report.scrbl") #:exists 'append
              (lambda ()
                (printf "@include-section[\"input/input.scrbl\"]\n\n")))

          (make-directory* scrbl_dir)

          (with-output-to-file
              scrbl_file
            (lambda ()
              (printf "#lang scribble/base\n\n")
              (printf "@title{Input}\n\n")
              (printf "collect input factors.\n")
              (printf "@section{Error Code Length: @bold{~a}}\n" error_code_length)
              (printf "@section{Bit Width: @bold{~a}}\n" bit_width)
              (printf "available bit width is 2 - 32\n")
              (printf "@section{Primitive Poly Value: @bold{~a}}\n" primitive_poly_value)
              (printf "encode and decode should use the same primitive poly.\n\n")
              (printf "@seclink{PrimitivePoly}\n")
              (printf "@section{Raw Data}\n")
              (printf (display-double-list
                       raw_list
                       (map (lambda (num) (~r #:base 2 #:min-width bit_width #:pad-string "0" num)) raw_list)
                       (add1 bit_width)
                       5))
              )))))

