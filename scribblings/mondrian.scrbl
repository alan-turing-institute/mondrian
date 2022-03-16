#lang scribble/manual
@require[scribble/example
         mondrian]
@require[@for-label[mondrian
                    racket/base]]

@(define the-eval
     (make-base-eval))
@(the-eval '(require mondrian))

@title{Mondrian}
@author[@author+email["James Geddes" "jgeddes@turing.ac.uk"]]

@defmodule[mondrian]

TODO:
look up defterm, deftech

@section{Overview}

Mondrian makes nicely formatted, textual tables.
@;{
|---+---|
| a | b |
|---+---|
| c | d |
|---+---|
}

@examples[#:eval the-eval
  (displayln
    (table-pretty-print
      (table-col
        (table-row "a" "b")
        (table-row "c" "d"))))]

Tables in mondrian are rectangular grids of rows and columns. The columns (and
rows) may be grouped so that the whole set of columns (or rows) has a tree
structure. Individual cells of the table may span several rows or columns.
@;{
|-------+-------|
|   A   |   B   |
|---+---+---+---|
| a : b | c : d |
|---+---+---+---|
| e : f | g : h |
|---+---+---+---|
}

@examples[#:eval the-eval
  (displayln
    (table-pretty-print
      (table-col
        (table-row "A" "B")
        (table-row (table-row "a" "b") (table-row "c" "d"))
        (table-row (table-row "e" "f") (table-row "g" "h")))))]

@section{Creating tables}

@defproc[(Table? [v any/c]) boolean?]{
Predicate to recognise tables.}

@defproc[(table-cell [v string?]
                     [#:rowspan    rowspan exact-nonnegative-integer? 0]
                     [#:colspan    colspan exact-nonnegative-integer? 0]
                     [#:align      alignment (or/c 'left 'right 'centre) 'left]
                     [#:fill-char  fill-char char? #\space]
                     [#:pad?       pad? boolean? #f]
                     [#:show-trim? show-trim? boolean? #f]) Table?]{
                     
Create a table consisting of a single cell, whose content is a string,
@racket[v].

A non-zero @racket[rowspan] (or @racket[colspan]) will ensure that the cell
spans that many lower-level rows (or columns) when it is combined into a larger
table.

A cell may end up formatted to a width that is wider or narrow than the natural
width of its content. When that happens, @racket[alignment] controls the
justification. If the width is wider than the natural width the cell is filled
with @racket[fill-char]; if the width is narrower than the natural width it is
truncated unless @racket[show-trim?] is @racket[#t], in which case some of the
content is replaced with periods. (However, periods will not replace more than
half the number of characters.)

TODO: WHAT DOES @racket[pad?] DO?}



@defproc[(table-row [tbl Table?] ...) Table?]
@defproc[(table-col) Table?]
@defproc[(table-rowwise-bind) Table?]
@defproc[(table-colwise-bind) Table?]




@section{Formatting}

@;@defproc[(table-format)]

@;@defproc[(table-pretty-print)]
@;@defproc[(make-standard-rule-maker)]


@section{Theory}


