#lang racket

(require "src/encode/encode.rkt")
(require "src/decode/decode.rkt")

(provide (contract-out
          [rs-encode (->* 
                      ((listof exact-integer?) natural?) 
                      (#:bit_width natural? #:primitive_poly_value natural?) 
                      (listof exact-integer?))]
          [rs-decode (->* 
                      ((listof exact-integer?) natural?) 
                      (#:bit_width natural? #:primitive_poly_value natural?)
                      (listof exact-integer?))]
          ))
