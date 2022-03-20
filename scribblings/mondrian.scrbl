#lang scribble/manual
@require[scribble/example
         scribble-math/dollar
         (prefix-in pict- pict) 
         pict/tree-layout
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

Mondrian can often tell which cells span multiple rows and columns. In the
example above, the cells marked `A' and `B' span two columns each, and the cell
marked `Y' spans two rows, even though these cells were not explicitly
constructed as spanning cells. In addition, the cell marked `G' also spans two
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


@section{Formatting tables}

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

@(use-mathjax)

The kinds of tables that mondrian deals with are two-dimensional, rectangular
arrays of cells where certain cells might span more than one row or
column. However, not all such arrangements are allowed: the rows (or columns)
spanned must be `consistent' throughout the table. In effect, there is a tree
structure on both the rows and the columns which the cells must respect. We call
such a consistent structure a `@tech{mondrian}.'

For example, the following two rectangular arrangements are @emph{not} legal
tables:
@verbatim{
|-------+-------|               |-------+-------|
|   A   |   B   |               |   A   |   B   |
|   :---+---+---|               |---+---+---+---|
|   : b | c : d |               | a : b | c : d | 
|---+---+---+---|               |---+---+---+---|
| e : f | g : h |               | e :   C   : h |
|---+---+---+---|               |---+---+---+---|
}
The problem with the first one is that cell `A' is not rectangular; the
problem with the second is that the column structure is `inconsistent' so that
cell `C' overlaps cells `A' and `B'.

On the other hand, the following table is legal:
@verbatim{
|---------+---------| 
|   aA    |   bA    | 
|----:----+----+----| 
|    : dC | eC : fC | 
| cB +----+----+----| 
|    : dD | eD : fD | 
|----+----+----+----| 
}
@(define (nd c)
   (pict-cc-superimpose (pict-disk 20 #:color "White" #:border-color "Gray")
                        (pict-text c)))

The columns have a hierarchical structure described by the tree:
@centered[
 (naive-layered
  (tree-layout #:pict (nd "")
    (tree-edge (tree-layout #:pict (nd "a")
                 (tree-edge (tree-layout #:pict (nd "c")))
                 (tree-edge (tree-layout #:pict (nd "d")))))
    (tree-edge (tree-layout #:pict (nd "b")
                 (tree-edge (tree-layout #:pict (nd "e")))
                 (tree-edge (tree-layout #:pict (nd "f")))))))]

and the rows by the tree:
@centered[
 (naive-layered
  (tree-layout #:pict (nd "")
    (tree-edge (tree-layout #:pict (nd "A")))
    (tree-edge (tree-layout #:pict (nd "B")
                 (tree-edge (tree-layout #:pict (nd "C")))
                 (tree-edge (tree-layout #:pict (nd "D")))))))]

Any legal cell in the table is identified by a pair consisting of a node of the
column structure and a node of the row structure. However, not every collection
of legal cells is a legal table: the cells must (a) not overlap; and (b) cover
the entire rectangular area.

Let @($ "R") and @($ "C") be finite trees (representing a row and column
structure respectively). For, say, @${r_1, r_2 \in R}, we write @${r_1 \leq r_2}
if @${r_1} is closer to the root of the tree than @${r_2}. Denote by @${R
\times_\leq C} the partial order on @${R\times C} given by the rule @${(r, c)
\leq (r', c')} if and only if @${r \leq r'} and @${c \leq c'}.


TODO: Fix the following, which is wrong.

A @deftech{mondrian} is a maximal antichain in @${R \times_\leq C}. That is, a
mondrian is a subset, @${M\subset R\times_\leq C}, such that:

@itemlist[#:style 'ordered

@item{No two elements of @${M} are comparable; and}

@item{Any other @${c\in R\times_\leq C} is comparable to some element of
@${M}.}]

The first condition ensures that no two cells overlap; the second ensures that,
taken together, the cells cover the entire table.

The functions @racket[table-row], @racket[table-col], and so on create new
tables by joining existing tables, either side-by-side or top-to-bottom. Two
tables joined in this way must have a consistent row- or column-structure. The
tables are said to be row-wise (or column-wise) @deftech{coherent} if one of
their row (or column) structures is a prefix of the other. In this case,
mondrian extends the smaller tree from the leaves.


