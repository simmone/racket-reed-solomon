#lang racket

(require "start.rkt")
(require "input/input.rkt")
(require "syndrome-poly/syndrome-poly.rkt")
(require "../../encode/express/primitive-poly/primitive-poly.rkt")
(require "euclidean-decode/euclidean-decode.rkt")

(provide (contract-out
          [express-start (-> void?)]
          [express-input (-> (listof exact-integer?) natural? natural? natural? void?)]
          [express-syndrome-poly (-> string? void?)]
          [express-primitive-poly (-> void?)]
          [express-euclidean-decode (-> void?)]
          ))

