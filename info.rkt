#lang info
(define collection "mondrian")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib" "pict-lib" "scribble-math"))
(define scribblings '(("scribblings/mondrian.scrbl" ())))
(define pkg-desc "Functional tables with text pretty-printing")
(define version "0.1")
(define pkg-authors '(jgeddes@turing.ac.uk))
