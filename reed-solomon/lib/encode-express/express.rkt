#lang racket

(require "header.rkt")
(require "input/input.rkt")
(require "primitive-poly/primitive-poly.rkt")
(require "galois-fields/galois-fields.rkt")
(require "generator-poly/generator-poly.rkt")
(require "message-poly/message-poly.rkt")
(require "long-division/long-division.rkt")

(provide (contract-out
          [write-report-header (-> path-string? void?)]
          [write-report-input (-> (listof exact-integer?) natural? natural? natural? path-string? void?)]
          [write-report-primitive-poly (-> path-string? void?)]
          [write-report-galois-fields (-> hash? path-string? void?)]
          [write-report-generator-poly (-> string? path-string? void?)]
          [write-report-message-poly (-> string? path-string? void?)]
          [write-report-long-division-start (-> path-string? void?)]
          [write-report-long-division-prepare-message (-> natural? string? path-string? void?)]
          ))
