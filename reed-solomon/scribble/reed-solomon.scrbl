#lang scribble/manual

@title{Reed-Solomon}

@author+email["Chen Xiao" "chenxiao770117@gmail.com"]

reed-solomon is a racket implementation for Reed Solomon Algorithm.

@link["https://en.wikipedia.org/wiki/Reed%E2%80%93Solomon_error_correction" "Reed–Solomon error correction"]

@table-of-contents[]

@section[#:tag "install"]{Install}

raco pkg install reed-solomon

@section{Usage}

@defmodule[reed-solomon]

@defproc[(rs-encode
           [data_list (listof exact-integer?)]
           [parity_length natural?]
           [#:bit_width bit_width natural? 8]
           [#:primitive_poly_value primitive_poly_value natural? 285]
           )
           (listof exact-integer?)]{

  data_list is a list of integer, each integer's range is 0 - 2^bit_width-1

  parity_length is user defined, (floor (/ parity_length 2)) is recovery capacity.

  ie: parity_length is 4, it can recover 2 symbols at most.

  bit_width: from 2 - 32

  primitive_poly_value: each bit width has multiple available primitive values.

  encode and decode should use the same primitive poly value.

  @link["https://github.com/simmone/racket-reed-solomon/blob/master/reed-solomon/src/primitive_poly_table.rkt" "2-32 bits available primitive value table"]
}

@defproc[(rs-decode
           [data_list (listof exact-integer?)]
           [parity_length natural?]
           [#:bit_width bit_width natural? 8]
           [#:primitive_poly_value primitive_poly_value natural? 285]
           )
           (listof exact-integer?)]{

  data_list is a list of data, appended rs code.

  parity_length, bit_width, primitive_poly_value should be consistent as encode.
}

@section{Example}

@codeblock{
#lang racket

(require reed-solomon)

(let* ([rs_code 
        (rs-encode
          '(1 2 3 4 5 6 7 8 9 10 11)
          4
          #:bit_width 4
          #:primitive_poly_value 19)]
       [polluted_data_list (append '(1 2 3 4 5 11 7 8 9 10 1) rs_code)])

  (printf "~a\n" 
    (rs-decode
      polluted_data_list
      4
      #:bit_width 4
      #:primitive_poly_value 19)))
  ;; (1 2 3 4 5 6 7 8 9 10 11 3 3 12 12)

(let* ([rs_code
        (rs-encode
          (bytes->list
            (string->bytes/utf-8 "Chen Xiao is just a programmer."))
            34)]
       [polluted_data_list
        (append
         (bytes->list #"Chen Xiao is a fabulous artist.")
         rs_code)])

  (printf "~a\n"
            (list->bytes
              (take (rs-decode polluted_data_list 34) 31))))
  ;; Chen Xiao is just a programmer.
}
