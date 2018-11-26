#lang racket

(provide (contract-out
          [get-gf256-hash (-> pair?)]
          [log-multiply (-> exact-integer? exact-integer? exact-integer?)]
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


(define (log-multiply a1 a2)
  (let* ([result (get-gf256-hash)]
         [aton_map (car result)])
    (if (>= (+ a1 a2) 255)
        (hash-ref aton_map (modulo (+ a1 a2) 255))
        (hash-ref aton_map (+ a1 a2)))))
