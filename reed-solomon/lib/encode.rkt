#lang racket

(require "poly.rkt")
(require "generate-poly.rkt")
(require "long-division.rkt")

(provide (contract-out
          [encode (-> string? natural? (listof natural?))]
          ))

(define (encode data patrity_length)
  (list 1))
