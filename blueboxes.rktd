536
((3) 0 () 1 ((q lib "mondrian/main.rkt")) () (h ! (equal) ((c def c (c (? . 0) q table-row)) q (695 . 3)) ((c def c (c (? . 0) q table-rowwise-bind)) q (859 . 3)) ((c def c (c (? . 0) q table-cell)) q (52 . 15)) ((c def c (c (? . 0) q make-standard-rule-maker)) q (1399 . 4)) ((c def c (c (? . 0) q table-col)) q (777 . 3)) ((c def c (c (? . 0) q Table?)) q (0 . 3)) ((c def c (c (? . 0) q table-format)) q (1021 . 3)) ((c def c (c (? . 0) q table-colwise-bind)) q (940 . 3)) ((c def c (c (? . 0) q table-pretty-print)) q (1107 . 7))))
procedure
(Table? v) -> boolean?
  v : any/c
procedure
(table-cell  v                             
            [#:rowspan rowspan             
             #:colspan colspan             
             #:align alignment             
             #:fill-char fill-char         
             #:pad? pad?                   
             #:show-trim? show-trim?]) -> Table?
  v : string?
  rowspan : exact-nonnegative-integer? = 0
  colspan : exact-nonnegative-integer? = 0
  alignment : (or/c 'left 'right 'centre) = 'left
  fill-char : char? = #\space
  pad? : boolean? = #f
  show-trim? : boolean? = #f
procedure
(table-row cell ...) -> Table?
  cell : (or/c Table? string?)
procedure
(table-col cell ...) -> Table?
  cell : (or/c Table? string?)
procedure
(table-rowwise-bind cells) -> Table?
  cells : (listof Table?)
procedure
(table-colwise-bind cells) -> Table?
  cells : (listof Table?)
procedure
(table-format table) -> (listof (listof string?))
  table : Table?
procedure
(table-pretty-print  table                         
                    [#:rule-maker rule-maker]) -> string?
  table : Table?
  rule-maker : (->
               exact-nonnegative-integer? exact-nonnegative-integer? char?)
             = (default-rule-maker)
procedure
(make-standard-rule-maker hchars vchars) -> procedure?
  hchars : string?
  vchars : string?
