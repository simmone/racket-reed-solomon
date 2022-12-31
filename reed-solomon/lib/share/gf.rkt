#lang racket

(require "poly.rkt")

(provide (contract-out
          [get-gf-aton-hash (-> natural? natural? hash?)]
          [poly-gf-a->n (-> hash? string? string?)]
          [poly-gf-n->a (-> hash? string? string?)]
          [poly-gf-a-multiply (->* (natural? string? string?) () #:rest (listof string?) string?)]
          [poly-gf-n-multiply (->* (natural? string? string?) () #:rest (listof string?) string?)]
          [poly-gf-n-divide-align (-> hash? hash? natural? string? string? string?)]
          [poly-gf-n-sub-x->a (-> hash? hash? natural? string? exact-integer? exact-integer?)]
          ))

(define (get-gf-aton-hash 2^m_1 primitive_poly_value)
  (let ([aton_map (make-hash)])
    (let loop ([a 0]
               [last_n (/ 1 2)])
      (when (< a 2^m_1)
            (let ([n (* last_n 2)])
              (when (> n 2^m_1)
                    (set! n (bitwise-xor n primitive_poly_value)))

              (hash-set! aton_map a n)

              (loop (add1 a) n))))
    aton_map))

(define (poly-gf-a->n gf_aton_map poly_str)
  (regexp-replace* #rx"a"
                   (poly-a->string
                    (map
                     (lambda (pair)
                       (cons (hash-ref gf_aton_map (car pair)) (cdr pair)))
                     (string-a->poly poly_str)))
                   ""))

(define (poly-gf-n->a gf_ntoa_map poly_str)
  (poly-a->string
   (map
    (lambda (pair)
      (cons (hash-ref gf_ntoa_map (car pair)) (cdr pair)))
    (filter (lambda (item) (not (= (car item) 0))) (string-n->poly poly_str)))))

(define poly-gf-a-multiply
  (lambda (2^m_1 poly1 poly2 . rst)
    (let loop ([loop_poly `(,poly2 ,@rst)]
               [last_poly poly1])
      (if (not (null? loop_poly))
          (loop
           (cdr loop_poly)
           (let inner-loop ([poly1_list (string-a->poly (car loop_poly))]
                            [result_poly '()])
             (if (not (null? poly1_list))
                 (inner-loop
                  (cdr poly1_list)
                  `(,@result_poly
                    ,@(map
                       (lambda (poly)
                         (cons 
                          (modulo (+ (caar poly1_list) (car poly)) 2^m_1)
                          (+ (cdar poly1_list) (cdr poly))))
                       (string-a->poly last_poly))))
                 (poly-a->string result_poly))))
          last_poly))))

(define poly-gf-n-multiply
  (lambda (poly1 poly2 . rst)
    (poly-gf-a->n
     (apply poly-gf-a-multiply
            (map
             (lambda (poly_n)
               (poly-gf-n->a poly_n))
             `(,poly1 ,poly2 ,@rst))))))

(define (poly-gf-n-divide-align gf_ntoa_map gf_aton_map 2^m_1 src dst)
  (let* ([src_pair (string-n->poly src)]
         [dst_pair (string-n->poly dst)]
         [src_n (caar src_pair)]
         [src_x (cdar src_pair)]
         [dst_n (caar dst_pair)]
         [dst_x (cdar dst_pair)]
         [src_a (hash-ref gf_ntoa_map src_n)]
         [dst_a (hash-ref gf_ntoa_map dst_n)])
    (format "~ax~a"
            (hash-ref gf_aton_map
                      (modulo
                       (- (+ 2^m_1 dst_a) src_a)
                       2^m_1))
            (- dst_x src_x))))

(define (poly-gf-n-sub-x->a gf_ntoa_map gf_aton_map 2^m_1 poly_n a)
  (hash-ref
   gf_ntoa_map
   (apply
    bitwise-xor
    (map
     (lambda (item)
       (hash-ref
        gf_aton_map
        (modulo (+ (car item) (* a (cdr item))) 2^m_1)))
     (string-a->poly (poly-gf-n->a poly_n))))))
