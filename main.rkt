
#lang racket/base

(require racket/match
         racket/list
         racket/string
         racket/function)

;; Pretty-printed tables

(provide table-cell 
         table-row
         table-col
         table-rowwise-bind
         table-colwise-bind
         table-format
         table-pretty-print
         make-standard-rule-maker
         )


(define *PAD-CHAR* #\space)
(define *ELIDED-CHAR* #\.)

#|
TODO: 
- Add cell formatting options to table-col and table-row
- Change names of exports
- Add contracts
- Consolidate tests
- Rules
- Break out Cells into different module
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
(define (table-cell v #:rowspan    [rowspan    0]
                      #:colspan    [colspan    0]
                      #:align      [alignment  'left]
                      #:fill-char  [fill-char  #\space]
                      #:pad?       [pad?       #f]
                      #:show-trim? [show-trim? #f])
  (define (shape-span n)
    (tree n (make-list n (tree-leaf 0))))
  (Table
   (list (Tile (cons null null)
               (Cell v alignment fill-char pad? show-trim?)))
   (shape-span rowspan) (shape-span colspan)))

;; Check whether a Table is a valid table
(define (table? t)
  (and (Table? t)
       (match-let ([(Table tiling row-shape col-shape) t])
         ;; TODO: Check that all table-indices are valid, given the row- and column-shapes
         (let ([table-indexes (map Tile-index tiling)])
           (and (is-anti-chain? table-indexes)
                (covers? table-indexes (cross-product (tree-leaves/index row-shape) (tree-leaves/index col-shape))))))))


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
      (tree (length col-shapes) col-shapes)))

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
      (tree (length row-shapes) row-shapes)))

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

(define (tree val subtrees)
  (cons val subtrees))

(define (tree-leaf val)
  (list val))

(define (tree-index<=? idx1 idx2)
  (list-prefix? idx1 idx2))

(define (tree-ref t idx)
  (cond
    [(null? idx) (car t)]
    [else        (tree-ref (list-ref (cdr t) (car idx)) (cdr idx))]))

;; Return a list of the labels along the branch indexed by idx
(define (branch-ref t idx)
  (cond
    [(null? idx) (list (car t))]
    [else (cons (car t) (branch-ref (list-ref (cdr t) (car idx)) (cdr idx)))]))

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

;; tree-leaves/branch-vals : return a list of each sequence of values along the branches leading to the
;; leaves
(define (tree-leaves/branch-values t)
  (let ([value    (car t)]
        [children (cdr t)])
    (cond
      [(null? children) (list (list value))]
      [else (map (curry cons value)
                 (append* (map tree-leaves/branch-values children)))])))

;; shape<=? : test whether one shape is a prefix of the other
;; shape1 <= shape2 if
;; - shape1 is a singleton; or
;; - the values are the same; and
;; - each child node of shape1 is a prefix (as a shape) of the corresponding child node of the other
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
    (tree val subtrees))
  
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
                       (map (λ (i) (tree-ref a-tree i)) (tree-leaves/index a-tree))))
    (check-equal? (branch-ref a-tree '(1 0)) '(a c d))
    (check-true (set=? (tree-leaves/branch-values a-tree)
                            '((a b) (a c d) (a c e))))))




;; ---------------------------------------------------------------------------------------------------
;; Formatted tables (version 0.1)
;;  
;; Lay out a table, with rules. 
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
(define (table-pretty-print tbl #:rule-maker [rule-maker (default-rule-maker)])
  (let ([printed-table (if (not rule-maker)
                           tbl
                           (table-add-rules tbl rule-maker))])
    (string-join
     (map string-append* (table-format printed-table))
     "\n"
     #:after-last "\n")))

;; Given a row index and a column index, return a character representing a rule
;; A row-depth of #f means return a vertical rule, and likewise for column-depth
;; if both arguments are not #f then return an "intersection" character
(define (make-standard-rule-maker* hchars vchars)
  (let ([hv (list->vector hchars)]
        [vv (list->vector vchars)])
    (λ (row col)
      (if (not col)
          (fenced-vector-ref hv (length row))
          (if (not row)
              (fenced-vector-ref vv (length col))
              (let ([r (length row)]
                    [c (length col)])
                (if (<= c r)
                    (fenced-vector-ref vv c)
                    (fenced-vector-ref hv r))))))))

;; Like vector-ref, but return the last element rather than out of range error
(define (fenced-vector-ref vec pos)
  (if (< pos (vector-length vec))
      (vector-ref vec pos)
      (vector-ref vec (- (vector-length vec) 1))))

(define (default-rule-maker)
  (make-standard-rule-maker "-" "|"))

;; make-vh-rule-maker : string? string? -> procedure?
(define (make-standard-rule-maker hchars vchars)
  (make-standard-rule-maker* (append (string->list hchars) '(#f))
                            (append (string->list vchars) '(#f))))

;; Add rules to a Table, producing a new (but improper!) Table. The new table may have rules which
;; overlap existing Tiles. However, they occur after the existing Tiles in tiling,  which means the
;; pretty-printer doesn't see them.  
;; table-add-rules : Table? proc? -> Table?
(define (table-add-rules tbl make-rule)
  (match-define (Table tiles row-shape col-shape) tbl)

  ;; Add rows and columns to hold the rules
  (define-values (new-row-shape new-rows updated-rows)
    (shape-add-between row-shape (λ (r) (make-rule r #f))))
  (define-values (new-col-shape new-cols updated-cols)
    (shape-add-between col-shape (λ (c) (make-rule #f c))))

  ;; Move the old tiles to their new row and col indices
  (define reindexed-tiles
    (for/list ([t (in-list tiles)])
      (match-define (Tile (cons row col) v) t)
      (Tile (cons (cdr (branch-ref updated-rows row))
                  (cdr (branch-ref updated-cols col)))
            v)))

  ;; The new horizontal rules
  (define horizontal-rule-tiles
    (for*/list ([row (in-list new-rows)]
                [col (in-list (map cdr (tree-leaves/branch-values updated-cols)))])  
      (Tile (cons row col) (cell "" #:fill-char (make-rule (drop-right row 1) #f)))))

  ;; The new vertical rules
  (define vertical-rule-tiles
    (for*/list ([row (in-list (map cdr (tree-leaves/branch-values updated-rows)))]
                [col (in-list new-cols)])
      (Tile (cons row col) (cell (string (make-rule #f (drop-right col 1)))))))

  ;; The new crossing rules
  (define horizontal-vertical-rule-tiles
    (for*/list ([row (in-list new-rows)]
                [col (in-list new-cols)])
      (Tile (cons row col) (cell (string (make-rule (drop-right row 1) (drop-right col 1)))))))

  (Table (append reindexed-tiles
                 horizontal-rule-tiles
                 vertical-rule-tiles
                 horizontal-vertical-rule-tiles)
         new-row-shape
         new-col-shape))

;; Interleave new leaves between the nodes of a shape, at all levels
;; Returns:
;; - (a) a new shape, including the interleaved nodes
;; - (b) a list of the indices of the new nodes, '((1) (3) (5) ...) plus the new sub-nodes
;; - (c) a mapping from old -> new indices
;; The mapping in (c) is a labelled tree whose structure is isomorphic to the original shape, but
;; whose labels are the updated index of each node
;;
;; splice? should be a function of an index. If splice? returns #f then no nodes are inserted between
;; the children of this particular node.
(define (shape-add-between shp splice?)
  (let-values ([(new-shape new-rows updated-children) (shape-add-between* shp '() splice?)])
    (values
     new-shape
     new-rows
     (tree #f updated-children))))

;; Like shape-add-between except (c) is a list of the updates for child nodes.
;; here : the (old) index of the current node, reversed
(define (shape-add-between* shp here splice?)
  (match-let ([(cons N nodes) shp])
    (cond
      [(zero? N) (values shp '() '())]
      [else
       (let ([splice-here? (splice? (reverse here))])
         ;; Update the children of this node
         (let-values ([(updated-children children-new-nodes children-updated-shapes)
                       (for/fold ([cs '()] 
                                  [ns '()]
                                  [us '()])
                                 ([node (in-list nodes)]
                                  [i    (in-naturals)])
                         (let-values ([(new-child new-nds new-updts)
                                       (shape-add-between* node (cons i here) splice?)])
                           (let ([ref (if splice-here? (* 2 i) i)])
                             (values (cons new-child cs)
                                     (append (map (λ (n) (cons ref n)) new-nds) ns)
                                     (cons (tree ref new-updts) us)))))])
           (if splice-here?
               ;; Add in new nodes 
               (values
                ;; (a) The new shape (with new nodes interleaved)
                (tree (- (* 2 N) 1) (interleave (reverse updated-children) (make-list (- N 1) (tree-leaf 0))))
                ;; (b) The indices of the added nodes
                (append (map list (range 1 (- (* 2 N) 1) 2)) children-new-nodes) 
                ;; (c) Map of old indices of existing nodes to their new indices, including this one
                (reverse children-updated-shapes))
               ;; Return the children, but don't add in new nodes
               (values ; else no new nodes added
                (tree N (reverse updated-children))
                children-new-nodes
                (reverse children-updated-shapes)))))])))


;; Interleave two lists; when one is exhausted, continue with the elements of the other
;; Recursive because xs and ys are likely to be short
(define (interleave xs ys)
  (if (null? xs)
      ys
      (if (null? ys)
          xs
          (cons (car xs) (cons (car ys) (interleave (cdr xs) (cdr ys)))))))



;; table-format : Table? -> [Listof [Listof string?]]
(define (table-format tbl)
  (match-let ([(Table tiles row-shape col-shape) tbl])
    (let ([row-heights (table-format-column-heights tbl)] ;; Ignored. TODO: All rows currently have
          ;; height 1. 
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
                       (format-cell (cell "") (tree-ref col-widths tile-col)))
                   (format-cell (cell "") 0))))))))))


;; Return a labelled tree whose labels are the computed row widths at each level
(define (table-format-row-widths tbl)
  (match-let ([(Table tiles _ col-shape) tbl])
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
  (match-let ([(Table _ row-shape _) tbl])
    (let sum-heights ([t row-shape])
      (let ([children (cdr t)])
        (cond
          [(null? children) (cons 1 '())]
          [else
           (let ([new-children (map sum-heights children)])
             (cons (apply + (map car new-children)) new-children))])))))

;; Make a boxtree (initialised to `val` at each node) having the same shape as a labelled tree `t`
(define (tree->boxtree t val)
  (tree (box val)
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
      (Tile '(() 0) (cell "a"))
      (Tile '(() 1) (cell "b"))
      (Tile '(() 2) (cell "c")))
     '(0)
     '(3 (0) (0) (0))))
  (define *tbl2*
    (Table
     (list
      (Tile '((0)) (cell "a"))
      (Tile '((1)) (cell "b"))
      (Tile '((2)) (cell "c")))
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
;; - aligned left, right, or centred
;; - optional single-character padding to right, left, or both (if aligned left, right, or centred respectively)
;; - optionally, showing that characters were omitted

(struct Cell (content
              alignment           ; either 'left, 'right, or 'centre
              fill-char           ; char?
              pad?                ; boolean?
              show-trim?          ; boolean? 
              ) #:transparent)

;; Convenience constructor with named, default arguments
(define (cell s
              #:align      [alignment  'left]
              #:fill-char  [fill-char  #\space]
              #:pad?       [pad?       #f]
              #:show-trim? [show-trim? #f]) 
  (Cell s alignment fill-char pad? show-trim?))

;; Produce the contents of c as a formatted string of given width
(define (format-cell c width)
  (let ([nw (cell-natural-width c)])
    (cond
      [(> width nw) (format-cell/fill c (- width nw))]
      [(< width nw) (format-cell/trim c width)]
      [else         (format-cell/asis c)])))

;; The width of cell just necessary to fit the contents and the padding
(define (cell-natural-width c)
  (+ (string-length (Cell-content c))
     (if (Cell-pad? c)
         (match (Cell-alignment c)
           ['left   1]
           ['right  1]
           ['centre 2])
         0)))

;; Format a cell into a space wider than its natural width
(define (format-cell/fill c n-fill)
  (let ([s       (Cell-content c)]
        [pad     (if (Cell-pad? c)
                     (string *PAD-CHAR*)
                     "")]
        [fill    (make-string n-fill (Cell-fill-char c))])
    (match (Cell-alignment c)
      ['left  (string-append s fill pad)]
      ['right (string-append pad fill s)]
      [else   (let ([n-lfill (quotient n-fill 2)])
                (let ([lfill (make-string n-lfill (Cell-fill-char c))]
                      [rfill (make-string (- n-fill n-lfill) (Cell-fill-char c))])
                  (string-append pad lfill s rfill pad)))])))

;; Format a cell into a space narrower than its natural width
;; A trim-character replaces the final 3 characters, unless there are fewer than 3 characters
;; remaining, in which case it replaces half the characters
;; Padding is not deleted until all the content has gone
;; Alignment is ignored, apart from its effect on padding (ie, the trimming is always done from the right)
(define (format-cell/trim c w)
  (if (not (Cell-pad? c))
      (format-cell/trim/no-pad c w)
      (let ([npad-chars
             (match (Cell-alignment c)
               [(or 'left 'right) 1]
               ['centre           2])])
        (if (<= w npad-chars)
            (make-string w *PAD-CHAR*)
            (let ([pad (string *PAD-CHAR*)]
                  [s (format-cell/trim/no-pad c (- w npad-chars))])
              (match (Cell-alignment c)
                ['left   (string-append s pad)]
                ['right  (string-append pad s)]
                ['centre (string-append pad s pad)]))))))
  
(define (format-cell/trim/no-pad c w)
  (let* ([s       (Cell-content c)]
         [n-elide (if (Cell-show-trim? c)
                      (min 3 (quotient w 2))
                      0)])  
    (string-append (substring s 0 (- w n-elide)) (elision n-elide))))

;; Format a string into precisely its natural width
(define (format-cell/asis c)
  (let ([s (Cell-content c)])
    (if (not (Cell-pad? c))
        s
        (let ([pad (string *PAD-CHAR*)])
          (match (Cell-alignment c)
            ['left    (string-append s pad)]
            ['right   (string-append pad s)]
            ['centre  (string-append pad s pad)])))))

;; A string containing the "elision" character
(define (elision n)
  (make-string n *ELIDED-CHAR*))



;; Tests for formatting cells
;; --------------------------

(module+ test
  (require rackunit)
  ;;
  (test-case "Format default cells"
    (define test-cell (cell "test"))
    (check-equal? (format-cell test-cell 4) "test")
    (check-equal? (format-cell test-cell 5) "test ")
    (check-equal? (format-cell test-cell 3) "tes"))
  ;;
  (test-case "Format padded cells"
    (define padded-cell (cell "test" #:pad? #t))
    (check-equal? (format-cell padded-cell 6) "test  ")
    (check-equal? (format-cell padded-cell 4) "tes ")
    (check-equal? (format-cell padded-cell 2) "t "))
  ;;
  (test-case "Format aligned cells"
   (define right-cell (cell "test" #:align 'right))
   (define centred-cell (cell "test" #:align 'centre))
   (check-equal? (format-cell right-cell 5) " test")
   (check-equal? (format-cell right-cell 3) "tes")
   (check-equal? (format-cell centred-cell 6) " test ")
   (check-equal? (format-cell centred-cell 5) "test ")
   (check-equal? (format-cell centred-cell 3) "tes"))
  ;;
  (test-case "Format trimmed cells"
    (define dots-cell (cell "alongtext" #:show-trim? #t))
    (define dots-cell-right (cell "alongtext" #:align 'right #:show-trim? #t))
    ;;
    (check-equal? (format-cell dots-cell 6) "alo...")
    (check-equal? (format-cell dots-cell 5) "alo..")
    (check-equal? (format-cell dots-cell 4) "al..")
    ;;
))
