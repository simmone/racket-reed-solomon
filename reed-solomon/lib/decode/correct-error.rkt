#lang racket

(require "../share/lib.rkt")
(require "../share/gf.rkt")
(require "../share/poly.rkt")
(require "../share/euclidean.rkt")

(provide (contract-out
          [correct-error (-> (listof exact-integer?) (listof pair?) (listof exact-integer?))]
          ))

(define (correct-error raw_list patch_list)
  (let loop ([patches patch_list]
             [loop_raw_list (reverse raw_list)])
    (if (not (null? patches))
        (loop
         (cdr patches)
         (list-set loop_raw_list 
                   (caar patches) 
                   (bitwise-xor (cdar patches) (list-ref loop_raw_list (caar patches)))))
        (reverse loop_raw_list))))
         
    
    