#lang racket

(provide (contract-out
          [get-gf256-hash (-> pair?)]
          [poly-multiply (-> string? string? string?)]
          [string->poly (-> string? (listof pair?))]
          [poly->string (-> (listof pair?) string?)]
          [poly-n->string (-> (listof pair?) string?)]
          [poly-a->n (-> string? string?)]
          [poly-combine-a (-> string? string?)]
          [poly-n->a (-> string? string?)]
          ))

(define (get-gf256-hash)
  (let ([aton_map (make-hash)]
        [ntoa_map (make-hash)])
    (let loop ([a 0]
               [last_n (/ 1 2)])
      (when (< a 255)
            (let ([n (* last_n 2)])
              (when (> n 255)
                    (set! n (bitwise-xor n 285)))

              (hash-set! aton_map a n)
              (hash-set! ntoa_map n a)
              (loop (add1 a) n))))
    (cons aton_map ntoa_map)))

(define (poly-a->n poly_str)
  (let* ([result (get-gf256-hash)]
         [aton_map (car result)])
    (regexp-replace* #rx"a"
                     (poly->string
                      (map
                       (lambda (pair)
                         (cons (hash-ref aton_map (car pair)) (cdr pair)))
                       (string->poly poly_str)))
                     "")))

(define (poly-combine-a poly_str)
  (let ([xa_map (make-hash)])
    (for-each
     (lambda (pair)
       (if (not (hash-has-key? xa_map (cdr pair)))
           (hash-set! xa_map (cdr pair) (car pair))
           (hash-set! xa_map (cdr pair) (bitwise-xor (car pair) (hash-ref xa_map (cdr pair))))))
     (string->poly poly_str))
    
    (poly->string (map (lambda (pair) (cons (cdr pair) (car pair))) (hash->list xa_map)))))

(define (poly-n->a poly_str)
  (let* ([result (get-gf256-hash)]
         [ntoa_map (cdr result)])
    (poly->string
     (map
      (lambda (pair)
        (cons (hash-ref ntoa_map (car pair)) (cdr pair)))
      (string->poly poly_str)))))

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
                 (cons 
                  (modulo (+ (caar poly1_list) (car poly)) 255)
                  (+ (cdar poly1_list) (cdr poly))))
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
                    (char-loop (cdr chars_list) 'ap pair_list)]
                   [(char=? (car chars_list) #\x)
                    (char-loop (cdr chars_list) 'xp (cons "0" pair_list))]
                   [(char-numeric? (car chars_list))
                    (let* ([items (regexp-match #rx"^([0-9]+).*" (list->string chars_list))]
                           [num (second items)])
                      (char-loop (list-tail chars_list (string-length num)) 'x (cons num pair_list)))]
                   [else
                    (char-loop (cdr chars_list) 'a pair_list)])]
                 [(eq? cursor 'ap)
                  (cond
                   [(char-numeric? (car chars_list))
                    (let* ([items (regexp-match #rx"^([0-9]+).*" (list->string chars_list))]
                           [num (second items)])
                      (char-loop (list-tail chars_list (string-length num)) 'x (cons num pair_list)))]
                   [(char=? (car chars_list) #\x)
                    (char-loop (cdr chars_list) 'xp (cons "1" pair_list))]
                   [else
                    (char-loop (cdr chars_list) 'ap pair_list)])]
                 [(eq? cursor 'x)
                  (cond 
                   [(char=? (car chars_list) #\x)
                    (char-loop (cdr chars_list) 'xp pair_list)]
                   [else
                    (char-loop (cdr chars_list) 'x pair_list)])]
                 [(eq? cursor 'xp)
                  (cond
                   [(char-numeric? (car chars_list))
                    (let* ([items (regexp-match #rx"^([0-9]+).*" (list->string chars_list))]
                           [num (second items)])
                      (char-loop null 'x (cons num pair_list)))]
                   [else
                    (char-loop (cdr chars_list) 'xp pair_list)])])
                (cond
                 [(null? pair_list)
                  (if (eq? cursor 'ap)
                      '(1 . 0)
                      '(0 . 0))]
                 [(= (length pair_list) 1)
                  (if (eq? cursor 'xp)
                      (cons (string->number (first pair_list)) 1)
                      (cons (string->number (first pair_list)) 0))]
                 [else
                  (cons (string->number (second pair_list))  (string->number (first pair_list)))])))
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

(define (poly-n->string poly_list)
  (regexp-replace* #rx"a" (poly->string poly_list) ""))
