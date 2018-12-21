#lang scribble/manual

@(require (for-label racket))

@title{Reed-Solomon}

@author+email["Chen Xiao" "chenxiao770117@gmail.com"]

reed-solomon is a racket implementation for Reed Solomon Algorithm.

@table-of-contents[]

@section[#:tag "install"]{Install}

raco pkg install reed-solomon

@section{Usage}

@defproc[(rs-encode
           [data_list (listof exact-integer?)]
           [parity_length natural?]
           [#:bit_width bit_width natural? 8]
           [#:primitive_poly_value primitive_poly_value natural? 285]
           [#:express? express? boolean? #f]
           [#:express_path express_path path-string? ".encode.express"]
           )
           (listof exact-integer?)]{

  data_list is a list of integer, each integer's range is 0 - 2^bit_width-1

  parity_length is user defined, (floor (/ parity_length 2)) is recovery capacity.
  ie: parity_length is 4, it can recover 2 symbols at most.

  bit_width: from 2 - 32

  primitive_poly_value: each bit width has multiple available primitive values.
  encode and decode should use the same primitive poly value.
  
  set #:express? to true will generate a detail report in express_path.
  into the express folder, @verbatim{scribble --htmls report.scrbl} to generate a detail report.

  Warning: express will generate a set of scribble files, it's very slow, debug usage only.
}

@defproc[(rs-decode
           [data_list (listof exact-integer?)]
           [parity_length natural?]
           [#:bit_width bit_width natural? 8]
           [#:primitive_poly_value primitive_poly_value natural? 285]
           [#:express? express? boolean? #f]
           [#:express_path express_path path-string? ".encode.express"]
           )
           (listof exact-integer?)]{

  data_list is a list of data, appended rs code.

  parity_length, bit_width, primitive_poly_value should be consistent as encode.
  
  set #:express? to true will generate a detail report in express_path.
  into the express folder, @verbatim{scribble --htmls report.scrbl} to generate a detail report.

  Warning: express will generate a set of scribble files, it's very slow, debug usage only.
}

@section{Example}

@verbatim{
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
