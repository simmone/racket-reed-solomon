#lang racket

(require "start.rkt")
(require "input/input.rkt")
(require "syndrome-poly/syndrome-poly.rkt")
(require "../../encode/express/primitive-poly/primitive-poly.rkt")
(require "../../encode/express/galois-fields/galois-fields.rkt")
(require "euclidean-decode/euclidean-decode.rkt")
(require "chien-search/chien-search.rkt")
(require "forney/forney.rkt")
(require "finally/finally.rkt")
(require "no-error/no-error.rkt")
(require "too-many-errors/too-many-errors.rkt")

(provide (contract-out
          [express-start (-> void?)]
          [express-input (-> (listof exact-integer?) natural? natural? natural? void?)]
          [express-syndrome-poly (-> string? void?)]
          [express-primitive-poly (-> void?)]
          [express-galois-fields (-> hash? hash? void?)]
          [express-euclidean-decode (-> void?)]
          [express-chien-search (-> (listof exact-integer?) void?)]
          [express-forney (-> string? string? string? string? (listof pair?) void?)]
          [express-finally (-> (listof exact-integer?) natural? void?)]
          [express-no-error (-> void?)]
          [express-too-many-errors (-> exn? void?)]
          ))

