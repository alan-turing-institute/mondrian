#lang scribble/manual
@require[scribble/example
         mondrian]
@require[@for-label[mondrian
                    racket/base]]

@(define my-evaluator (make-base-eval))

@title{Mondrian}
@author{James Geddes}

@defmodule[mondrian] makes nicely formatted textual tables. Here is an example table:

@examples[#:eval my-evaluator
(table-pretty-print (


Tables in @racket[mondrian] are rectangular grids of rows and columns. Both the
rows and the columns may be grouped so that the whole set of rows (or columns)
has a treee structure. The individual entries of the table may span several rows 



