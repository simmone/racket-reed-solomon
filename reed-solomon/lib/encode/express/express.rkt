#lang racket

(require "start.rkt")
(require "input/input.rkt")
(require "primitive-poly/primitive-poly.rkt")
(require "galois-fields/galois-fields.rkt")
(require "generator-poly/generator-poly.rkt")
(require "message-poly/message-poly.rkt")
(require "error-code/error-code.rkt")

(provide (contract-out
          [express-start (-> void?)]
          [express-input (-> (listof exact-integer?) natural? natural? natural? void?)]
          [express-primitive-poly (-> void?)]
          [express-galois-fields (-> hash? hash? void?)]
          [express-generator-poly (-> string? void?)]
          [express-message-poly (-> string? void?)]
          [express-error-code (-> natural? (listof exact-integer?) void?)]
          ))
