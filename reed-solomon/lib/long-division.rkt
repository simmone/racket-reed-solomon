#lang racket

(require "poly.rkt")

(provide (contract-out
          [message->poly (-> string? string?)]
          [prepare-message (-> string? natural? string?)]
          [prepare-generator (-> string? natural? string?)]
          ))

(define (message->poly msg)
  (let ([char_list (string->list msg)])
    (with-output-to-string
      (lambda ()
        (let loop ([loop_list char_list]
                   [count (sub1 (length char_list))])
          (when (not (null? loop_list))
                (if (= count 0)
                    (printf "~ax0" (char->integer (car loop_list)))
                    (printf "~ax~a+" (char->integer (car loop_list)) count))
                (loop (cdr loop_list) (sub1 count))))))))

(define (prepare-message msg count)
  (poly-a->n
   (poly-multiply
    (poly-n->a msg)
    (format "x~a" count))))

(define (prepare-generator generator count)
  (let ([first_x (cdar (string->poly generator))])
    (poly-multiply
     generator (format "x~a" (- count first_x)))))
   
