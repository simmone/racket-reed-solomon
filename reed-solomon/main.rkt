#lang racket

(require "lib/encode/encode.rkt")
(require "lib/decode/decode.rkt")

(provide (contract-out
          [rs-encode (->* 
                      ((listof exact-integer?) natural?) 
                      (#:bit_width natural? #:primitive_poly_value natural? #:express? boolean? #:express_path path-string?) 
                      (listof exact-integer?))]
          [rs-decode (->* 
                      ((listof exact-integer?) natural?) 
                      (#:bit_width natural? #:primitive_poly_value natural? #:express? boolean? #:express_path path-string?) 
                      (listof exact-integer?))]
          ))
