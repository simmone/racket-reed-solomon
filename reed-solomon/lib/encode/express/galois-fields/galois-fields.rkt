#lang racket

(require "../../../share/lib.rkt")

(provide (contract-out
          [express-galois-fields (-> hash? hash? void?)]
          ))

(define (express-galois-fields aton_map ntoa_map)
  (when (*express?*)
        (let* ([scrbl_dir (build-path (*express_path*) "galois-fields")]
               [scrbl_file (build-path scrbl_dir "galois-fields.scrbl")])

          (with-output-to-file
              (build-path (*express_path*) "report.scrbl") #:exists 'append
              (lambda ()
                (printf "@include-section[\"galois-fields/galois-fields.scrbl\"]\n\n")))

          (make-directory* scrbl_dir)

          (with-output-to-file
              scrbl_file
            (lambda ()
              (printf "#lang scribble/base\n\n")
              (printf "@title{Appendix. Galois Fields}\n\n")
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
              )))))

