#lang racket

(provide (contract-out
          [poly-multiply (-> string? string? string?)]
          [string->poly (-> string? (listof pair?))]
          [poly->string (-> (listof pair?) string?)]
          ))

(define (poly-multiply poly1 poly2)
  (let ([poly2_list (string->poly poly2)])
    (let loop ([poly1_list (string->poly poly1)]
               [result_poly '()])
      (if (not (null? poly1_list))
          (loop
           (cdr poly1_list)
           `(,@result_poly
             ,@(map
               (lambda (poly)
                 (cons (+ (caar poly1_list) (car poly)) (+ (cdar poly1_list) (cdr poly))))
               poly2_list)))
          (poly->string result_poly)))))

(define (string->poly poly_str)
  (let loop ([loop_list (regexp-split #rx"\\+|\\-" poly_str)]
             [result_list '()])
    (if (not (null? loop_list))
        (loop
         (cdr loop_list)
         (cons
          (let char-loop ([chars_list (string->list (car loop_list))]
                          [cursor 'a]
                          [pair_list '()])
            (if (not (null? chars_list))
                (cond
                 [(eq? cursor 'a)
                  (cond
                   [(char=? (car chars_list) #\a)
                    (char-loop (cdr chars_list) 'a pair_list)]
                   [(char-numeric? (car chars_list))
                    (char-loop (cdr chars_list) 'x (cons (car chars_list) pair_list))]
                   [(char=? (car chars_list) #\x)
                    (char-loop (cdr chars_list) 'x (cons #\0 pair_list))])]
                 [(eq? cursor 'x)
                  (cond 
                   [(char=? (car chars_list) #\x)
                    (char-loop (cdr chars_list) 'x pair_list)]
                   [(char-numeric? (car chars_list))
                    (char-loop null 'x (cons (car chars_list) pair_list))])])
                (cond
                 [(null? pair_list)
                  '(0 . 0)]
                 [(= (length pair_list) 1)
                  (cons (string->number (string (first pair_list))) 0)]
                 [else
                  (cons (string->number (string (second pair_list)))  (string->number (string (first pair_list))))])))
          result_list))
        (reverse result_list))))

(define (poly->string poly_list)
  (let ([sorted_list
         (map
          (lambda (poly)
            (format "a~ax~a" (car poly) (cdr poly)))
          (sort
           poly_list
           (lambda (poly1 poly2)
             (if (not (= (cdr poly1) (cdr poly2)))
                 (> (cdr poly1) (cdr poly2))
                 (>= (car poly1) (car poly2))))))])
    
    (foldr
     (lambda (poly1 poly2)
       (if (null? poly2)
           poly1
           (string-append poly1 "+" poly2)))
     null
     sorted_list)))

