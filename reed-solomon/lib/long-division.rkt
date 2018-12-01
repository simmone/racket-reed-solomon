#lang racket

(require "poly.rkt")

(provide (contract-out
          [message->poly (-> string? string?)]
          [prepare-message (-> string? natural? string?)]
          [prepare-generator (-> string? natural? string?)]
          [poly-xor (-> string? string? string?)]
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

(define (poly-xor long_poly short_poly)
  (poly-n->string
   (let loop ([long_list (string->poly long_poly)]
              [short_list (string->poly short_poly)]
              [result_list '()])
     (if (not (null? long_list))
         (if (null? short_list)
             (loop (cdr long_list) null (cons (cons (bitwise-xor (caar long_list) 0) (cdar long_list)) result_list))
             (loop (cdr long_list) (cdr short_list) (cons (cons (bitwise-xor (caar long_list) (caar short_list)) (cdar long_list)) result_list)))
         (reverse result_list)))))
