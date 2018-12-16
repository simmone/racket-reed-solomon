#lang racket

(provide (contract-out
          [string-a->poly (-> string? (listof pair?))]
          [string-n->poly (-> string? (listof pair?))]
          [poly-a->string (-> (listof pair?) string?)]
          [poly-n->string (-> (listof pair?) string?)]
          [poly-n-combine (-> string? string?)]
          [poly-n-add (-> string? string? string?)]
          [poly-n-car (-> string? pair?)]
          [poly-n-tail (-> string? pair?)]
          [poly-n->coeffients (-> string? (listof natural?))]
          [coeffients->poly-n (-> (listof natural?) string?)]
          ))

(define (string-a->poly poly_str)
  (string->poly poly_str 'a))

(define (string-n->poly poly_str)
  (string->poly poly_str 'n))

(define (string->poly poly_str aorn)
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
                    (char-loop (cdr chars_list) 'xp (if (eq? aorn 'a) (cons "0" pair_list) (cons "1" pair_list)))]
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

(define (poly-a->string poly_list)
  (poly->string poly_list 'a))

(define (poly-n->string poly_list)
  (poly->string poly_list 'n))

(define (poly->string poly_list aorn)
  (if (null? poly_list)
      ""
      (let ([sorted_list
             (map
              (lambda (poly)
                (format "~a~ax~a"
                        (if (eq? aorn 'a) "a" "")
                        (car poly)
                        (cdr poly)))
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
         sorted_list))))

(define (poly-n-combine poly_str)
  (let ([xa_map (make-hash)])
    (for-each
     (lambda (pair)
       (if (not (hash-has-key? xa_map (cdr pair)))
           (hash-set! xa_map (cdr pair) (car pair))
           (hash-set! xa_map (cdr pair) (bitwise-xor (car pair) (hash-ref xa_map (cdr pair))))))
     (string-a->poly poly_str))
    
    (poly-n->string (map (lambda (pair) (cons (cdr pair) (car pair))) (hash->list xa_map)))))

(define (poly-n-add poly1_n poly2_n)
  (poly-n->string
   (let loop ([loop_list 
               (string-n->poly (poly-n-combine (poly-n->string `(,@(string-n->poly poly1_n) ,@(string-n->poly poly2_n)))))])
     (if (not (null? loop_list))
         (if (= (caar loop_list) 0)
             (loop (cdr loop_list))
             loop_list)
         '()))))

(define (poly-n-car poly_str)
  (car (string-n->poly poly_str)))

(define (poly-n-tail poly_str)
  (car (take-right (string-n->poly poly_str) 1)))

(define (poly-n->coeffients poly_n_str)
  (map
   (lambda (item)
     (car item))
   (string-n->poly poly_n_str)))

(define (coeffients->poly-n raw_list)
  (with-output-to-string
    (lambda ()
      (let loop ([loop_list raw_list]
                 [count (sub1 (length raw_list))])
          (when (not (null? loop_list))
                (if (= count 0)
                    (printf "~ax0" (car loop_list))
                    (printf "~ax~a+" (car loop_list) count))
                (loop (cdr loop_list) (sub1 count)))))))
