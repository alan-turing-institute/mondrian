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

Mondrian makes nicely formatted, textual tables.

@section{Overview}

Tables in mondrian are rectangular grids of rows and columns. The columns (and
rows) may be grouped so that the whole set of columns (or rows) has a tree
structure. Individual cells of the table may span several rows or columns.
@;{
|---+---|
| a | b |
|---+---|
| c | d |
|---+---|

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
        (table-row "a" "b")
        (table-row "c" "d"))))

   (displayln
     (table-pretty-print
       (table-col
         (table-row "X"
           (table-row (table-cell "A" #:align 'right)
                      (table-cell "B" #:align 'right)))
         (table-row
           "Y"
           (table-col (table-row (table-row "a" "b") (table-row "c" "d")) 
                      (table-row (table-row "e" "f") "G"))))
     #:rule-maker (make-standard-rule-maker "-" "|:")))
]

Mondrian can often tell which cells span multiple rows and columns. Note that in
the example above, the cells marked `A' and `B' span two columns each, and the
cell marked `Y' spans two rows. Furthermore, the cell marked `G' also spans two
columns even though it is not a traditional `column header.'

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

A cell may end up formatted to a width that is wider or narrower than the
natural width of its content. When that happens, @racket[alignment] controls the
justification. If the width is wider than the natural width the cell is filled
with @racket[fill-char]; if the width is narrower than the natural width it is
truncated unless @racket[show-trim?] is @racket[#t], in which case some of the
content is replaced with periods. (However, periods will not replace more than
half the number of characters.)

If @racket[pad?] is @racket[#t], an additional space is added on the non-justified
side (or both sides if @racket[alignment] is @racket['centre]).
}


@defproc[(table-row [cell (or/c Table? string?)] ...) Table?]{

Join multiple tables together to make a single row. As a convenience, if any
@racket[cell] is a string, it is converted to a @racket[Table?] using
@racket[table-cell] with the default arguments.

The individual cells must be @tech{coherent} tables.
}

@defproc[(table-col [cell (or/c Table? string?)] ...) Table?]{ Join multiple
tables together to make a single column. As a convenience, if any @racket[cell]
is a string, it is converted to a @racket[Table?] using @racket[table-cell] with
the default arguments.

The individual cells must be @tech{coherent} tables.
}

@defproc[(table-rowwise-bind [cells (listof Table?)]) Table?]{
Combine the tables in @racket[cells], which must be @tech{coherent}, into a column.
}

@defproc[(table-colwise-bind [cells (listof Table?)]) Table?]{
Combine the
tables in @racket[cells], which must be @tech{coherent}, into a column.
}


@section{Formatting}

@defproc[(table-format [table Table?]) (listof (listof string?))]{

Format @racket[table] into a list of rows, where each row is a list of strings. Cells
which span multiple columns or rows may be padded (either horizontally or vertically)
or trimmed (horizontally only). Note that if some cells span columns, the row lists
may not all have the same length; likewise, if a cell spans rows, then that cell
may be present in multiple rows.

The outcome is that the lowest-level cells will have their natural sizes whereas
row and column `headers' will have their size adjusted to match. 

The actual rules governing the final widths of each cell are as follows (the
rules governing the heights of columns are identical, @italic{mutatis
mutandis}). The @defterm{natural width} of a cell is the length of its contents;
the @defterm{natural height} of a cell is always 1.

@itemlist[#:style 'ordered

@item{If this is a leaf cell, then assign it its natural width.}

@item{Otherwise, if all cells below this (in the column hierarchy) have zero
width, then the cell has its natural width.}

@item{Otherwise, the cell has a width equal to the sum of its spanned columns.}]

}

@defproc[(table-pretty-print [table Table?] [#:rule-maker rule-maker (->
exact-nonnegative-integer? exact-nonnegative-integer? char?)
(default-rule-maker)]) string?]{

Format @racket[table] as a single string, including line breaks, with row and column `rules.'

Rules are characters which are inserted between columns and between rows. The
particular character that is inserted depends upon the `depth' of the rows or
columns to either side. The default rule-maker is equivalent to
@racket[(make-standard-rule-maker "-" "|")] and inserts `@tt{|}' between the
columns and `@tt{-}' between the rows; intersections use the `highest-level'
rule and ties are broken in favour of vertical rules.

Note that rules are not placed around the edges of the table by default. To have
outside rules, the table should have empty cells to the left and right, or top
and bottom.

In general, @racket[rule-maker] should be a procedure that accepts two
non-negative integers, the row depth and the column depth, and returns either a
@racket[char?] or @racket[#f]. If the row depth (respectively, the column depth)
is @racket[#f] then the procedure should return a vertical (respectively,
horizontal) separator. If both are non-false, then the procedure should return
an intersection character. The procedure may return @racket[#f] in which case no
rule is inserted.

Internally, @racket[table-pretty-print] creates a new table by splicing in cells
containing the rules and then running the result through @racket[table-format].
}


@defproc[(make-standard-rule-maker [hchars string?] [vchars string?])
procedure?]{

Returns a procedure suitable for passing to @racket[table-pretty-print] to
generate the appropriate column- and row-separators.

@racket[hchars] should be a string of characters, from `highest' to `lowest' in
the hierarchy of columns; likewise for @racket[vchars].
}




@section{Theory}


