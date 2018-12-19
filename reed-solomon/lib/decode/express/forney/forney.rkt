#lang racket

(require "../../../share/lib.rkt")

(provide (contract-out
          [express-forney (-> string? string? (listof pair?) void?)]
          ))

(define (express-forney lam_derivative_poly Yj_poly err_correct_pairs)
  (when (*express?*)
        (let* ([scrbl_dir (build-path (*express_path*) "forney")]
               [scrbl_file (build-path scrbl_dir "forney.scrbl")])

          (with-output-to-file
              (build-path (*express_path*) "report.scrbl") #:exists 'append
              (lambda ()
                (printf "@include-section[\"forney/forney.scrbl\"]\n\n")))

          (make-directory* scrbl_dir)

          (with-output-to-file
              scrbl_file
            (lambda ()
              (printf "#lang scribble/base\n\n")
              (printf "@title{Forney Algorithm}\n\n")
              (printf "find error correction throuth forney algorithm.\n")
              (printf "@section{Derivative Lambda Poly}\n")
              (printf "@verbatim{~a}\n" lam_derivative_poly)
              (printf "@section{omega / derivative}\n")
              (printf "@verbatim{omega poly / derivative lambda poly = ~a}\n" Yj_poly)
              (printf "@section{Error Calculation Example}\n")
              (printf "@verbatim{error correction = a^i * (replace last poly's x with a^-i)}\n")
              (printf "@verbatim{for example:}\n")
              (printf "@verbatim{poly = 10x+2, error place is 9}\n")
              (printf "@verbatim{=a^9*(10*(a^-9)1+2)=a^9*(10*a(15-9)+2)}\n")
              (printf "@verbatim{=a^(9+9+6) + a^(1+9)=a^(24 % 15) + a^10=a^9 + a^10}\n")
              (printf "@verbatim{=10 ^ 7}\n")
              (printf "@verbatim{=13}\n\n")
              (printf "@section{Error Correction Pairs}\n")
              (printf (display-list
                       err_correct_pairs
                       10
                       10))
              )))))

