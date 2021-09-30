#lang racket/base

(require racket/match
         racket/list
         racket/string)

;; Pretty-printed tables

(provide table-cell 
         table-row
         table-col
         table-rowwise-bind
         table-colwise-bind
         table-format
         table-pretty-print
         )


(define PAD-CHAR #\space)
(define ELIDED-CHAR #\.)

#|
TODO: 
- Add cell formatting options to table-col and table-row
  . and perhaps these should be for values only?
- Change names of exports
- Add contracts
- Consolidate tests

|#



#|

Tables
------

A Table is a rectangular arrangement of cells, where the cells may span several rows or columns, but
the rows / columns have a common hierarchical structure.

A table-index is a pair of tree-indexes.

A tile is a pair of a table-index and a value.

A shape is a labelled ordered tree, whose labels are the number of children of that node (for
efficiency, since length takes time proportional to the length).

A table is
- a row shape and a column shape
- a set of tilings; and
such that:
(a) No tile index covers another
(b) Every pair of a leaf of a row-tree-index and a leaf of a column-tree-index is
covered by one tile index

|#

(struct Table (tiling row-shape col-shape) #:transparent)
(struct Tile (index value) #:transparent)

;; There is no empty Table
;;(define *table-empty* (Table null null null))

(define (table-index<=? idx1 idx2)
  (and (tree-index<=? (car idx1) (car idx2))
       (tree-index<=? (cdr idx1) (cdr idx2))))

;; A Table consisting of a single cell
(define (table-cell v #:rowspan    [rowspan 0]
                      #:colspan    [colspan 0]
                      #:position   [position   'left]
                      #:margin/h   [margin/h   0]
                      #:show-trim? [show-trim? #f])
  (define (shape-span n)
    (cons-tree n (make-list n (tree-leaf 0))))
  (Table
   (list (Tile (cons null null) (cell v position margin/h show-trim?)))
   (shape-span rowspan) (shape-span colspan)))

;; Check whether a Table is a valid table
(define (table? t)
  (and (Table? t)
       (match-let ([(Table tiling row-shape col-shape) t])
         ;; TODO: Check that all table-indices are valid, given the row- and column-shapes
         (let ([table-indexes (map Tile-index tiling)])
           (and (is-anti-chain? table-indexes)
                (covers? table-indexes (cross-product (tree-leaves/index row-shape) (tree-leaves/index col-shape))))))))

;; Remove rows and columns that have no tile 
(define (table-prune tbl)
  (raise-user-error 'table-prune "Function not yet implemented"))



;; Two Tables are rowwise- (respectively, columnwise-) consistent if their row shapes (respectively, column
;; shapes) are the same. Two Tables are rowwise- (respectively, columnwise-) coherent if one row shape
;; (respectively, column shape) is a prefix of the other.


;; Make a Table from a (non-empty) row of rowwise-coherent tables placed side-by-side, creating a new root.
;;
;;                               X
;;                            / |  \   
;;    A   B     C            A  B    C 
;;        |    / \     =>       |   / \
;;        b   c   c'            b  c   c' 
;;
(define (table-colwise-bind tbls)
    
  ;; The final row shape. NB: shape-max will raise an error if the shapes are not coherent. 
  (define row-shape
    (with-handlers ([exn:fail:user?
                     (λ (e) (raise-user-error 'table-colwise-bind (exn-message e)))])
      (apply shape-max (map Table-row-shape tbls))))

  ;; The final column shape
  (define col-shape
    (let ([col-shapes (map Table-col-shape tbls)])
      (cons-tree (length col-shapes) col-shapes)))

  ;; Renumber the tiles
  ;; For each tile in table n,
  ;; - prepend n to the column index; and
  ;; - keep the row index the same
  (define (indexifier tile n)
    (match-let ([(Tile (cons row col) val) tile])
      (Tile (cons row (cons n col)) val)))
  
  (define tiling
    (reindex-tiles-by-table indexifier tbls))  

  (Table tiling row-shape col-shape))


;; Make a Table from a (non-empty) column of columnwise-coherent tables placed top-to-bottom
;;
(define (table-rowwise-bind tbls)

  ;; The final row shape.
  (define row-shape
    (let ([row-shapes (map Table-row-shape tbls)])
      (cons-tree (length row-shapes) row-shapes)))

  ;; The final column shape. NB: shape-max will raise an error if the shapes are not coherent. 
  (define col-shape
    (with-handlers ([exn:fail:user?
                     (λ (e) (raise-user-error 'table-rowwise-bind (exn-message e)))])
      (apply shape-max (map Table-col-shape tbls))))

  ;; Renumber the tiles
  ;; For each tile in table n,
  ;; - keep the column index the same; and
  ;; - prepend n to the row index
  (define (indexifier tile n)
    (match-let ([(Tile (cons row col) val) tile])
      (Tile (cons (cons n row) col) val)))
  
  (define tiling
    (reindex-tiles-by-table indexifier tbls))

  (Table tiling row-shape col-shape))

(define (reindex-tiles-by-table indexifier tbls)
  (flatten
   (for/list ([i  (in-naturals)]
              [ts (in-list (map Table-tiling tbls))])
     (map (λ (t) (indexifier t i)) ts))))


;; table-row
;; Create a table row using table-rowwise-bind. Values (cells that are not a Table?) are first promoted
;; using table-cell
(define (table-row cell . cells)
  (table-colwise-bind (cellulise (cons cell cells))))

;; table-col
;; Create a table row using table-rowwise-bind. Values (cells that are not a Table?) are first promoted
;; using table-cell
(define (table-col cell . cells)
  (table-rowwise-bind (cellulise (cons cell cells))))

(define (cellulise cells)  
  (map (λ (c)
         (cond
           [(Table? c) c]
           [else (table-cell c)]))
       cells))


;; Table-related low-level functions
;; ---------------------------------

;; Check that no tbl-idx covers one of the others
(define (is-anti-chain? tbl-idxs)
  (or 
   (null? tbl-idxs)
   (match-let ([(cons idx idxs) tbl-idxs])
     (and (no-overlaps idx idxs)
          (is-anti-chain? idxs)))))

;; Check that idx neither covers nor is covered by any of the idxs 
  (define (no-overlaps idx idxs)
    (andmap
     (λ (i)
       (not (or (table-index<=? i idx) (table-index<=? idx i)))
       idxs)))
  
  ;; Check that all indexes in "cells" are covered by some index in tble-idxs
  (define (covers? tbl-idxs cells)
    (andmap
     (λ (cell)
       (ormap (λ (idx) (table-index<=? cell idx)) tbl-idxs))
     cells))
  
;; Make a list of all ordered pairs of ms and ns 
(define (cross-product ms ns)
  (for*/list ([m ms]
              [n ns])
    (cons m n)))


#|

Labelled ordered trees
----------------------

A labelled ordered tree is represented as a pair, where the car is the label and the cdr is a list of
the children of that node

An "index" of a node in a tree is represented as a list of integers, which are the indexes of the
subtrees of each node, in order. The index of the root node is the empty list.

We say node1 "covers" node2 if node1 <= node2 (that is, n1 is closer to the root than n2 and on the
same branch).

A "boxtree" is a labelled, ordered tree, where the labels are boxes.

|#

(define (cons-tree val subtrees)
  (cons val subtrees))

(define (tree-leaf val)
  (list val))

(define (tree-index<=? idx1 idx2)
  (list-prefix? idx1 idx2))

(define (tree-ref t idx)
  (cond
    [(null? idx) (car t)]
    [else        (tree-ref (list-ref (cdr t) (car idx)) (cdr idx))]))

;; Assumes the reference at idx is a box
(define (tree-set! t idx val)
  (cond
    [(null? idx) (set-box! (car t) val)]
    [else        (tree-set! (list-ref (cdr t) (car idx)) (cdr idx) val)]))

(define (is-valid-tree-ref? t idx)
  (or
   (null? idx) 
   (match-let ([(cons val children) t])
     (and (< (car idx) (length children))
          (is-valid-tree-ref? (list-ref children (car idx)) (cdr idx))))))


;; tree-leaves : return the labels of the leaves of the tree, in tree-order
(define (tree-leaves/value t)
  (define (tree-leaves* t acc)
    (let ([children (cdr t)])
      (cond
        [(null? children) (cons (car t) acc)]
        [else             (foldr tree-leaves* acc children)])))
  (tree-leaves* t null))

;; leaf-indexes : return the indexes of the leaves (ignoring the values)
(define (tree-leaves/index t)
  (define (tree-leaves* t here acc)
    (let ([children (cdr t)])
      (cond
        [(null? children) (cons (reverse here) acc)]
        [else             (let* ([N (length children)]
                                 [locs (map cons (range N) (make-list N here))])
                            (foldr tree-leaves* acc children locs))])))
  (tree-leaves* t '() '()))

;; shape<=? : test whether one shape is a prefix of the other
;; shape1 <= shape2 if
;; - shape1 is a singleton; or
;; - the values are the same; and
;; - each child node of shape1 is a prefix (as a shape) the corresponding child node of the other
;; Note that this is not the same condition as for a tree. In particular, if a node exists in shape1 and has at
;; least one child; then all children must exist in shape2.  
;;
(define (shape<=? s1 s2)
  (or
   (zero? (car s1))
   (and
    (= (car s1) (car s2))
    (andmap shape<=? (cdr s1) (cdr s2)))))

;; There is no empty shape
(define *unit-shape* (cons 0 null))

;; Error if s1 and s2 are incomparable
(define (shape-max/2 s1 s2)
  (cond
    [(shape<=? s1 s2) s2]
    [(shape<=? s2 s1) s1]
    [else (raise-user-error 'shape-max/2 "Expected coherent tables as arguments")]))

(define (shape-max . ss)
  (foldl shape-max/2 *unit-shape* ss))


;; Tests for trees
;; ---------------

(module+ test
  (require rackunit
           racket/set)

  (define (make-tree val . subtrees)
    (cons-tree val subtrees))
  
  (test-case "Tree tests"
    ;;      a
    ;;     / \
    ;;    b   c
    ;;       / \
    ;;      d   e
    (define a-tree
      (make-tree 'a
                 (tree-leaf 'b)
                 (make-tree 'c
                            (tree-leaf 'd) (tree-leaf 'e))))
    ;;
    (check-true (is-valid-tree-ref? a-tree '()))
    (check-true (is-valid-tree-ref? a-tree '(1 0)))
    (check-eq? (tree-ref a-tree '()) 'a)
    (check-eq? (tree-ref a-tree '(1 0)) 'd)
    (check-true (set=? (tree-leaves/value a-tree)
                       (map (λ (i) (tree-ref a-tree i)) (tree-leaves/index a-tree))))))




;; ---------------------------------------------------------------------------------------------------
;; Formatted tables (version 0.1)
;;  
;; Lay out a table, with no rules. Main challenge is to assign widths to leaf nodes of the column
;; shape; and heights to the leaf nodes of the row shape.
;;
;; Column shape:
;; - Leaf nodes get their maximum of the natural widths of all leaf nodes in that columns;
;; - Every other node gets a width equal to the sum of those below it (trimming if necessary)
;; - Except that there may be leaf nodes that don't have a corresponding
;; tile, in which case the columns above them get their natural width, and the leaves are filled. 
;;
;; Row shape:
;; - Leaf rows have height 1; every other row has height equal to the sum of its children. 

;; table-pretty-print : Table? -> string?
(define (table-pretty-print tbl)
  (string-join
   (map string-append* (table-format tbl))
   "\n"
   #:after-last "\n"))

;; table-format : Table? -> [Listof [Listof string?]]
(define (table-format tbl)
  (match-let ([(Table tiles row-shape col-shape) tbl])
    (let ([row-heights (table-format-column-heights tbl)] ;; TODO: We don't currently use `rows`
          [col-widths  (table-format-row-widths tbl)])
      ;; Loop over the atomic cells of the table
      (for/list ([row (in-list (tree-leaves/index row-shape))])
        (for/list ([col (in-list (tree-leaves/index col-shape))])
          ;; Find the tile that covers this row and col (there is exactly one, by the definition of a tiling)
          (let ([tile
                 (findf (λ (tile) (table-index<=? (Tile-index tile) (cons row col))) tiles)])
            ;; Are we in the top-left of this cell? If so, emit the contents.
            ;; Otherwise, if we're in the left-most column, emit padding to the appropriate width;
            ;; Otherwise, emit an empty string
            ;; To find, eg, which row we're in, take the remainder of row after deleting
            ;; the prefix given by the table-row index (noting that it must be a prefix). If that
            ;; suffix is empty, then this is the only row. Otherwise, we are in the top-most row if
            ;; all subsequent entries are 0.
            (match-let ([(cons tile-row tile-col) (Tile-index tile)])
              (let ([row-within-tile (list-tail row (length tile-row))]
                    [col-within-tile (list-tail col (length tile-col))])
               (if (andmap zero? col-within-tile)
                   (if (andmap zero? row-within-tile)
                       (format-cell (Tile-value tile) (tree-ref col-widths tile-col))
                       (format-cell (cell "" 'left 0 #f) (tree-ref col-widths tile-col)))
                   (format-cell (cell "" 'left 0 #f) 0))))))))))


;; Return a labelled tree whose labels are the computed row widths at each level
(define (table-format-row-widths tbl)
  (match-let ([(Table tiles row-shape col-shape) tbl])
    ;; Assign each node in a col-shape the maximum of all widths of tiles at that level 
    (define *widths* (tree->boxtree col-shape 0))
    (for ([tile (in-list tiles)])
      (let ([col-idx (cdr (Tile-index tile))]
            [wd      (cell-natural-width (Tile-value tile))])
        (tree-set! *widths* col-idx (max wd (unbox (tree-ref *widths* col-idx))))))
    ;; Now re-assign the widths to be either:
    ;; - the natural width, if either this is a leaf or the sum of all lower widths are zero;
    ;; - otherwise, the sum of the lower widths
    (let sum-widths! ([t *widths*])
      (let ([lower-widths (apply + (map sum-widths! (cdr t)))])
        (cond
          [(zero? lower-widths) (unbox (car t))]
          [else                 (begin
                                  (set-box! (car t) lower-widths)
                                  lower-widths)])))
    (boxtree->tree *widths*)))

;; Return a labelled tree whose labels are the column heights at each level
(define (table-format-column-heights tbl)
  (match-let ([(Table tiles row-shape col-shape) tbl])
    (let sum-heights ([t row-shape])
      (let ([children (cdr t)])
        (cond
          [(null? children) (cons 1 '())]
          [else
           (let ([new-children (map sum-heights children)])
             (cons (apply + (map car new-children)) new-children))])))))

;; Make a boxtree (initialised to `val` at each node) having the same shape as a labelled tree `t`
(define (tree->boxtree t val)
  (cons-tree (box val)
             (map (λ (c) (tree->boxtree c val)) (cdr t))))

(define (boxtree->tree bt)
  (cons (unbox (car bt))
        (map boxtree->tree (cdr bt))))


;; Test for tables
;; ---------------

(module+ test
  (require rackunit)
  (define *tbl1*
    (Table
     (list
      (Tile '(() 0) (cell "a" 'left 0 #f))
      (Tile '(() 1) (cell "b" 'left 0 #f))
      (Tile '(() 2) (cell "c" 'left 0 #f)))
     '(0)
     '(3 (0) (0) (0))))
  (define *tbl2*
    (Table
     (list
      (Tile '((0)) (cell "a" 'left 0 #f))
      (Tile '((1)) (cell "b" 'left 0 #f))
      (Tile '((2)) (cell "c" 'left 0 #f)))
     '(3 (0) (0) (0))
     '(0)))
  (check-equal? (table-row "a" "b" "c") *tbl1*)
  (check-equal? (table-col "a" "b" "c") *tbl2*))

(module+ test
  (require rackunit)
  (define *table*
    (table-rowwise-bind
     (list
      (table-colwise-bind
       (list
        (table-cell "H1")
        (table-cell "H2" #:colspan 2)))
      (table-rowwise-bind
       (list
        (table-colwise-bind
         (list
          (table-cell "r1") (table-row "x1" "y1")))
        (table-colwise-bind
         (list
          (table-cell "r2") (table-row "x2" "y2"))))))))
  (check-equal? (table-format *table*)
                (list (list "H1" "H2  " "")
                      (list "r1" "x1" "y1")
                      (list "r2" "x2" "y2")))

  )




;; ---------------------------------------------------------------------------------------------------
;;
;; Formatted cells
;;
;; Format single-row cells to a given width, trimming or padding as required
;; - possibly with a margin
;; - left, right, or centred
;; - possibly showing that characters were omitted

(struct Cell (content
              position            ; either 'left, 'right, or 'centre
              margin/h            ; exact-nonnegative-integer?
              show-trim?          ; boolean? 
              ) #:transparent)

;; A Cell having a default format
(define (cell s position margin/h show-trim?) 
  (Cell s position margin/h show-trim?))


;; Produce the contents of c as a formatted string of given width
(define (format-cell c width)
  (let ([nw (cell-natural-width c)])
    (cond
      [(> width nw) (format-cell/fill c (- width nw))]
      [(< width nw) (format-cell/trim c width)]
      [else         (format-cell/asis c)])))

;; The width of cell just necessary to fit the contents and the margin
(define (cell-natural-width c)
  (+ (string-length (Cell-content c))
     (* 2 (Cell-margin/h c))))

;; Format a cell into a space wider than its natural width
(define (format-cell/fill c n-fill)
  (let ([s      (Cell-content c)]
        [margin (padding (Cell-margin/h c))])
    (match (Cell-position c)
      ['left  (string-append margin                   s (padding n-fill) margin)]
      ['right (string-append margin (padding n-fill)  s                  margin)]
      [else   (let ([n-lfill (quotient n-fill 2)])
                (string-append
                             margin (padding n-lfill) s (padding (- n-fill n-lfill)) margin))])))

;; Format a cell into a space narrower than its natural width
;; Margins are not deleted until all the contents have gone
(define (format-cell/trim c w)
  (let* ([n-margin (Cell-margin/h c)]
         [n-s      (- w (* 2 n-margin))])
    (cond
      [(<= n-s 0) (padding w)] ; only margin is left after trim
      [else 
       (define margin (padding n-margin))
       (define s      (Cell-content c))
       (define slen   (string-length s))
       (define n-elide
         (if (Cell-show-trim? c)
             (min 3 (quotient n-s 2))
             0))  
       (match (Cell-position c)
         ['right (string-append margin (elision n-elide) (substring s (- slen (- n-s n-elide))) margin)]
         [else   (string-append margin (substring s 0 (- n-s n-elide)) (elision n-elide) margin)])])))

;; Format a string into precisely its natural width
(define (format-cell/asis c)
  (let ([margin (make-string (Cell-margin/h c) PAD-CHAR)])
    (string-append margin
                   (Cell-content c)
                   margin)))

;; A string containing the pad character
(define (padding n)
  (make-string n PAD-CHAR))

;; A string containing the "elision" character
(define (elision n)
  (make-string n ELIDED-CHAR))



;; Tests for formatting cells
;; --------------------------

(module+ test
  (require rackunit)
  ;;
  (test-case "Format default cells"
    (define test-cell (cell "test" 'left 0 #f))
    (check-equal? (format-cell test-cell 4) "test")
    (check-equal? (format-cell test-cell 5) "test ")
    (check-equal? (format-cell test-cell 3) "tes"))
  ;;
  (test-case "Format padded cells"
    (define padded-cell (cell "test" 'left 2 #f))
    (check-equal? (format-cell padded-cell 6) "  te  ")
    (check-equal? (format-cell padded-cell 7) "  tes  ")
    (check-equal? (format-cell padded-cell 3) "   "))
  ;;
  (test-case "Format justified cells"
   (define right-cell (cell "test" 'right 0 #f))
   (define centred-cell (cell "test" 'centre 0 #f))
   (check-equal? (format-cell right-cell 5) " test")
   (check-equal? (format-cell right-cell 3) "est")
   (check-equal? (format-cell centred-cell 6) " test ")
   (check-equal? (format-cell centred-cell 5) "test ")
   (check-equal? (format-cell centred-cell 3) "tes"))
  ;;
  (test-case "Trimmed cells"
    (define dots-cell (cell "alongtext" 'left 0 #t))
    (define dots-cell-right (cell "alongtext" 'right 0 #t))
    ;;
    (check-equal? (format-cell dots-cell 6) "alo...")
    (check-equal? (format-cell dots-cell 5) "alo..")
    (check-equal? (format-cell dots-cell 4) "al..")
    ;;
    (check-equal? (format-cell dots-cell-right 6) "...ext")
    (check-equal? (format-cell dots-cell-right 5) "..ext")
    (check-equal? (format-cell dots-cell-right 4) "..xt")))
