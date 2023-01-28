#lang racket

(require "../../src/field-math.rkt")
(require "../../src/lib/lib.rkt")

;; Galios multiply on GF(16)

(define gf16 '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))

(print-line `(,"" ,@gf16)) 

(parameterize*
 ([*field_generator_poly* "x4+x+1"])
 (let loop ([items gf16])
   (when (not (null? items))
     (print-line
      `(
        ,(car items)
        ,@(map (lambda (val) (galios-multiply (car items) val)) gf16)))
     (loop (cdr items)))))

      
