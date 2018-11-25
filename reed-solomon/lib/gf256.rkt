#lang racket

(provide (contract-out
          [gf256-aton (-> exact-integer? exact-integer?)]
          ))

(define (gf256-aton a)
  (let ([val (expt 2 a)])
    (if (>= val 256)
        (bitwise-xor val 285)
        val)))
