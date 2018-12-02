#lang racket

(require "poly.rkt")

(provide (contract-out
          [message->poly (-> string? string?)]
          [prepare-message (-> string? natural? string?)]
          [prepare-generator (-> string? natural? string?)]
          [poly-n-xor (-> string? string? string?)]
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

(define (poly-n-xor poly1 poly2)
  (poly-n->string
   (let loop ([poly1_list (string->poly poly1)]
              [poly2_list (string->poly poly2)]
              [result_list '()])
     (if (or (not (null? poly1_list)) (not (null? poly2_list)))
         (cond
          [(null? poly1_list)
           (loop null (cdr poly2_list) (cons (cons (bitwise-xor 0 (caar poly2_list)) (cdar poly2_list)) result_list))]
          [(null? poly2_list)
           (loop (cdr poly1_list) null (cons (cons (bitwise-xor (caar poly1_list) 0) (cdar poly1_list)) result_list))]
          [else
           (loop (cdr poly1_list) (cdr poly2_list) (cons (cons (bitwise-xor (caar poly1_list) (caar poly2_list)) (cdar poly1_list)) result_list))])
         (reverse result_list)))))
  
