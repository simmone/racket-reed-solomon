#lang racket

(require "header.rkt")

(provide (contract-out
          [write-report-header (-> path-string? void?)]
          ))
