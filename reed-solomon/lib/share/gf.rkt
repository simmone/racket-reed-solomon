#lang racket

(require "poly.rkt")

(provide (contract-out
          [get-gf-aton-hash (-> hash?)]
          [poly-gf-a->n (-> string? string?)]
          [poly-gf-n->a (-> string? string?)]
          [poly-gf-multiply (-> string? string? string?)]
          [*bit_width* parameter?]
          [*2^m_1* parameter?]
          [*primitive_poly_value* parameter?]
          [*gf_aton_map* parameter?]
          [*gf_ntoa_map* parameter?]
          ))

(define *bit_width* (make-parameter #f))
(define *2^m_1* (make-parameter #f))
(define *primitive_poly_value* (make-parameter #f))
(define *gf_aton_map* (make-parameter #f))
(define *gf_ntoa_map* (make-parameter #f))

(define (get-gf-aton-hash)
  (let ([aton_map (make-hash)])
    (let loop ([a 0]
               [last_n (/ 1 2)])
      (when (< a (*2^m_1*))
            (let ([n (* last_n 2)])
              (when (> n (*2^m_1*))
                    (set! n (bitwise-xor n (*primitive_poly_value*))))

              (hash-set! aton_map a n)

              (loop (add1 a) n))))
    aton_map))

(define (poly-gf-a->n poly_str)
  (regexp-replace* #rx"a"
                   (poly->string
                    (map
                     (lambda (pair)
                       (cons (hash-ref (*gf_aton_map*) (car pair)) (cdr pair)))
                     (string->poly poly_str)))
                   ""))

(define (poly-gf-n->a poly_str)
  (poly->string
   (map
    (lambda (pair)
      (cons (hash-ref (*gf_ntoa_map*) (car pair)) (cdr pair)))
    (string->poly poly_str))))

(define (poly-gf-multiply poly1 poly2)
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
                  (modulo (+ (caar poly1_list) (car poly)) (*2^m_1*))
                  (+ (cdar poly1_list) (cdr poly))))
               poly2_list)))
          (poly->string result_poly)))))
