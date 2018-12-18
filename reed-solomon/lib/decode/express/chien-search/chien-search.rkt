#lang racket

(require "../../../share/lib.rkt")

(provide (contract-out
          [express-chien-search (-> (listof exact-integer?) void?)]
          ))

(define (express-chien-search error_place_list)
  (when (*express?*)
        (let* ([scrbl_dir (build-path (*express_path*) "chien-search")]
               [scrbl_file (build-path scrbl_dir "chien-search.scrbl")])

          (make-directory* scrbl_dir)

          (with-output-to-file
              scrbl_file
            (lambda ()
              (printf "#lang scribble/base\n\n")
              (printf "@title{Chien Search}\n\n")
              (printf "find error occur places throuth chien search.\n")
              (printf "@section{Error Places Found}\n")
              (printf (display-list
                       error_place_list
                       5
                       5))
              )))))

