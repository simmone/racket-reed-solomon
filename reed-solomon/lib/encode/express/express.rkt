#lang racket

(require "start.rkt")
(require "input/input.rkt")
(require "primitive-poly/primitive-poly.rkt")
(require "galois-fields/galois-fields.rkt")
(require "generator-poly/generator-poly.rkt")
(require "message-poly/message-poly.rkt")
(require "long-division/long-division.rkt")
(require "error-code/error-code.rkt")

(provide (contract-out
          [express-header (-> path-string? void?)]
          [express-input (-> (listof exact-integer?) natural? natural? natural? path-string? void?)]
          [express-primitive-poly (-> path-string? void?)]
          [express-galois-fields (-> hash? hash? path-string? void?)]
          [express-generator-poly (-> string? path-string? void?)]
          [express-message-poly (-> string? path-string? void?)]
          [express-long-division-start (-> path-string? void?)]
          [express-long-division-detail (-> natural? string? string? natural? natural? string? natural? string? string? string? string? natural? path-string? void?)]
          [express-error-code (-> natural? (listof exact-integer?) path-string? void?)]
          ))
