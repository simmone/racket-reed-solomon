#lang racket

(require "header.rkt")
(require "input/input.rkt")
(require "primitive-poly/primitive-poly.rkt")

(provide (contract-out
          [write-report-header (-> path-string? void?)]
          [write-report-input (-> (listof exact-integer?) natural? natural? natural? path-string? void?)]
          [write-report-primitive-poly (-> path-string? void?)]
          ))
