#lang racket

(provide (contract-out
          [express (-> boolean? procedure? void?)]
          ))

(define (express express? proc)
  (when express?
        (proc)))
