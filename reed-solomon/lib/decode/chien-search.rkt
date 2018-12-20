#lang racket

(require "../share/lib.rkt")
(require "../share/gf.rkt")
(require "../share/poly.rkt")
(require "../share/euclidean.rkt")

(provide (contract-out
          [chien-value (-> string? natural? exact-integer?)]
          [chien-search (-> string? (listof exact-integer?))]
          ))

(define (chien-value lam_poly gf_seq)
  (foldr
   bitwise-xor
   0
  (map
   (lambda (item)
     (hash-ref 
      (*gf_aton_map*)
      (modulo
       (+
        (car item)
        (* (- (*2^m_1*) gf_seq) (cdr item)))
       (*2^m_1*))))
   (string-a->poly (poly-gf-n->a lam_poly)))))

(define (chien-search lam_poly)
  (let loop ([loop_index (*2^m_1*)]
             [result_list '()])
    (if (>= loop_index 0)
        (let ([chien_value (chien-value lam_poly loop_index)])
          (if (= chien_value 0)
              (loop (sub1 loop_index) (cons loop_index result_list))
              (loop (sub1 loop_index) result_list)))
        (reverse result_list))))
      
