#lang scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sudoku puzzles are a simple and popular amusement given as a
;;; nine-by-nine grid of cells, some of them containing digits:
;;; 
;;; ┌──────────┬──────────┬──────────┐
;;; │┌──┬──┬──┐│┌──┬──┬──┐│┌──┬──┬──┐│
;;; ││7 │  │  │││1 │  │  │││  │  │  ││
;;; │├──┼──┼──┤│├──┼──┼──┤│├──┼──┼──┤│
;;; ││  │2 │  │││  │  │  │││  │1 │5 ││
;;; │├──┼──┼──┤│├──┼──┼──┤│├──┼──┼──┤│
;;; ││  │  │  │││  │  │6 │││3 │9 │  ││
;;; │└──┴──┴──┘│└──┴──┴──┘│└──┴──┴──┘│
;;; ├──────────┼──────────┼──────────┤
;;; │┌──┬──┬──┐│┌──┬──┬──┐│┌──┬──┬──┐│
;;; ││2 │  │  │││  │1 │8 │││  │  │  ││
;;; │├──┼──┼──┤│├──┼──┼──┤│├──┼──┼──┤│
;;; ││  │4 │  │││  │9 │  │││  │7 │  ││
;;; │├──┼──┼──┤│├──┼──┼──┤│├──┼──┼──┤│
;;; ││  │  │  │││7 │5 │  │││  │  │3 ││
;;; │└──┴──┴──┘│└──┴──┴──┘│└──┴──┴──┘│
;;; ├──────────┼──────────┼──────────┤
;;; │┌──┬──┬──┐│┌──┬──┬──┐│┌──┬──┬──┐│
;;; ││  │7 │8 │││5 │  │  │││  │  │  ││
;;; │├──┼──┼──┤│├──┼──┼──┤│├──┼──┼──┤│
;;; ││5 │6 │  │││  │  │  │││  │4 │  ││
;;; │├──┼──┼──┤│├──┼──┼──┤│├──┼──┼──┤│
;;; ││  │  │  │││  │  │1 │││  │  │2 ││
;;; │└──┴──┴──┘│└──┴──┴──┘│└──┴──┴──┘│
;;; └──────────┴──────────┴──────────┘
;;; 
;;; The challenge is to fill the empty cells with the digits 1 through 9 in
;;; such a way that no row, column, or three-by-three sub-grid contains the
;;; same digit two or more times.
;;; 
;;; Write a program to solve sudoku puzzles; your program may assume the
;;; puzzle is well-formed. What is the solution of the above puzzle?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require scheme/control)
(require srfi/54 srfi/26)

(define-syntax assert
  (syntax-rules ()
    [(_ op a b)
     (unless (op a b)
       (error "Assertion failure" 'op 'a 'b))]))

;;; Solve the sample problem.
(define (main)
  (solve-work (make-constraints "7..1......2.....15.....639.2...18....4..9..7....75...3.785.....56.....4......1..2")
	      print-solution))

;;; And a sample "extreme" problem.
(define (main-extreme)
  (solve-work (make-constraints "..9748...7.........2.1.9.....7...24..64.1.59..98...3.....8.3.2.........6...2759..")
	      print-solution))

;;; Build the constraints for an n^2 by n^2 sudoku puzzle.
(define (board-constraints nsqrt)
  (define n (sqr nsqrt))
  (define range-n (in-range 1 (add1 n)))
  (define range-nsqrt (in-range 1 (add1 nsqrt)))
  (define (make-row row col piece)
    (+ (* 100 row) (* 10 col) piece))
  (append
    ;; Each cell only to have a single number.
    (for*/list ([row range-n]
		[col range-n])
      (let ([pieces (for/list ([piece range-n]) (make-row row col piece))])
	(make-column (format "u~a~a" row col) pieces)))

    ;; Each number only once per row.
    (for*/list ([row range-n]
		[piece range-n])
      (let ([row-vals (for/list ([col range-n]) (make-row row col piece))])
	(make-column (format "r~a-p~a" row piece) row-vals)))

    ;; Each number only once per column.
    (for*/list ([col range-n]
		[piece range-n])
      (let ([col-vals (for/list ([row range-n]) (make-row row col piece))])
	(make-column (format "c~a-p~a" col piece) col-vals)))

    ;; Each number only once per group.
    (for*/list ([gy (in-range nsqrt)]
		[gx (in-range nsqrt)]
		[piece range-n])
      (let ([group-vals (for*/list ([dy (in-range nsqrt)]
				    [dx (in-range nsqrt)])
			  (make-row (+ (* nsqrt gy) dy 1)
				    (+ (* nsqrt gx) dx 1)
				    piece))])
	(make-column (format "g~a~a-p~a" (add1 gx) (add1 gy) piece) group-vals)))))

;;; Convert an initial board into a set of given constraints.
(define (given-constraints initial)
  (define n (sqrt (string-length initial)))
  (assert = (sqr n) (string-length initial))
  (for*/list ([row (in-range n)]
	      [col (in-range n)]
	      [offset (in-value (+ (* n row) col))]
	      [ch (in-value (string-ref initial offset))]
	      #:when (char<=? #\1 ch #\9))
    (let ([piece (- (char->integer ch) (char->integer #\0))])
      (make-column (format "i~a~a-p~a" (add1 col) (add1 row) piece)
		   (list (+ (* 100 (add1 row))
			    (* 10 (add1 col))
			    piece))))))

;;; Given a list of rows of a solution, print out the board.  Assumes
;;; that the row numbers will sort properly, and that the solution is
;;; complete.
(define (print-solution answer)
  (define alength (length answer))
  (define sorted-answer (sort answer <))
  (define n (sqrt alength))
  (define nsub (sqrt n))
  (define vlimit (* n nsub))
  (define hsep (make-string (* 2 nsub) #\-))
  (define hline (string-join (make-list nsub hsep) "-+"))
  (assert = (sqr n) (length answer))
  (assert = (sqr nsub) n)
  (for ([cell (in-list sorted-answer)]
	[col (in-cycle (in-range 1 (add1 n)))]
	[hbox (in-cycle (in-range 1 (add1 nsub)))]
	[vbox (in-cycle (in-range 1 (add1 vlimit)))]
	[fullc (in-range 1 (add1 alength))])
    (printf " ~a" (remainder cell 10))
    (when (and (= hbox nsub) (< col n))
      (display " |"))
    (when (= col n)
      (newline))
    (when (and (= vbox vlimit) (< fullc alength))
      (display hline)
      (newline))))

;;; Construct constraints for a given problem.
(define (make-constraints initial)
  (define root (create-node))
  (define n (sqrt (sqrt (string-length initial))))
  (assert = (sqr (sqr n)) (string-length initial))
  (for-each (cut insert-column root <>) (board-constraints n))
  (for-each (cut insert-column root <>) (given-constraints initial))
  (sever-row-helper! root)
  root)

(define (node-writer item port write?)
  (display "<node>" port))
(define (column-writer item port write?)
  (fprintf port "<col ~a ~a>" (ncolumn-name item) (ncolumn-size item)))
(define (row-writer item port write?)
  (fprintf port "<row ~a>" (nrow-row item)))
(define (cell-writer item port write?)
  (fprintf port "<cell ~a>" (ncell-row item)))

;;; General node, contains the links.
(define-struct node (up down left right)
  #:mutable
  #:property prop:custom-write node-writer)

;;; Column header node.
(define-struct (ncolumn node) (name [size #:mutable])
  #:property prop:custom-write column-writer)
(define (ncolumn-adjust-size! node incr)
  (set-ncolumn-size! node (+ (ncolumn-size node) incr)))

;;; Row header node.  Only used during construction.
(define-struct (nrow node) (row)
  #:property prop:custom-write row-writer)

;;; Regular cell.  Column points to column node, 'row' is just the
;;; number of this row.
(define-struct (ncell node) (row column)
  #:property prop:custom-write cell-writer)

;;; The link chain through nodes is symmetrical for left/right and
;;; up/down.  These accessors manipulate the nodes in a more general
;;; manner.  The axis is either 'horizontal or 'vertical, and the polarity is
;;; 'prior or 'next.

(define (node-link node axis polarity)
  ((case axis
     [(horizontal) (case polarity
	     [(prior) node-left]
	     [(next) node-right]
	     [else (error "Invalid polarity" polarity)])]
     [(vertical) (case polarity
	     [(prior) node-up]
	     [(next) node-down]
	     [else (error "Invalid polarity" polarity)])]
     [else (error "Invalid axis" axis)])
   node))

(define (set-node-link! node axis polarity value)
  ((case axis
     [(horizontal) (case polarity
	     [(prior) set-node-left!]
	     [(next) set-node-right!]
	     [else (error "Invalid polarity" polarity)])]
     [(vertical) (case polarity
	     [(prior) set-node-up!]
	     [(next) set-node-down!]
	     [else (error "Invalid polarity" polarity)])]
     [else (error "Invalid axis" axis)])
   node value))

(define (flip-polarity polarity)
  (case polarity
    [(next) 'prior]
    [(prior) 'next]
    [else (error "Invalid polarity" polarity)]))

;;; Construct a single column, of a given name, and containing the
;;; mentioned rows.
(define (make-column name rows)
  (define head (create-column name (length rows)))
  (for ([row-num (in-list rows)])
    (let ([cell (create-cell row-num head)])
      (node-insert! head cell 'vertical 'prior)))
  head)

;;; Insert this column into the root, adding rows used in the column
;;; as necessary.
(define (insert-column root head)
  (node-insert! root head 'horizontal 'prior)
  (let loop ([rcell (node-down root)]
	     [cell (node-down head)])
    ;(printf "loop: rcell:~a, cell:~a~%" rcell cell)
    (cond
      [(not (ncell? cell))
       ;(display "Done!\n")
       (void)]
      [(or (not (nrow? rcell))
	   (> (nrow-row rcell) (ncell-row cell)))
       ;(display "Insert!\n")
       (let ([new-rcell (create-row (ncell-row cell))])
	 (node-insert! rcell new-rcell 'vertical 'prior)
	 (node-insert! new-rcell cell 'horizontal 'prior)
	 ;(ensure-links new-rcell 'vertical)
	 ;(ensure-links new-rcell 'horizontal)
	 ;(ensure-links cell 'vertical)
	 ;(ensure-links cell 'horizontal)
	 (loop (node-down new-rcell) (node-down cell)))]
      [(= (nrow-row rcell) (ncell-row cell))
       ;(display "Already\n")
       (node-insert! rcell cell 'horizontal 'prior)
       ;(ensure-links rcell 'vertical)
       ;(ensure-links rcell 'horizontal)
       ;(ensure-links cell 'vertical)
       ;(ensure-links cell 'horizontal)
       (loop (node-down rcell) (node-down cell))]
      [else (loop (node-down rcell) cell)])))

;;; Remove the row helper column from the table.
(define (sever-row-helper! root)
  (let loop ([row (node-down root)])
    (when (nrow? row)
      (node-remove! row 'vertical)
      (node-remove! row 'horizontal)
      (loop (node-down row)))))

;;; Find the first lowest sized column in the table, or #f if there
;;; are no columns in the table.
(define (find-smallest-column root)
  (let loop ([smallest #f]
	     [column (node-right root)])
    (if (ncolumn? column)
      (loop (if (or (not smallest)
		    (< (ncolumn-size column) (ncolumn-size smallest)))
	      column smallest)
	    (node-right column))
      smallest)))

;;; First piece of advice.  Go to the wikipedia article on Dancing
;;; Links.  Print it out, and burn it.  Then find the link to Knuth's
;;; paper on the algorithm, read it, and things will make lots of
;;; sense.  The Wikipedia article is completely wrong.

;;; First, some syntax to help us iterate.  This evalutes the body in
;;; for all of the nodes along a given axis, _not_ counting the one
;;; passed in.
(define-syntax for-each-node
  (syntax-rules ()
    [(_ (id start axis polarity) body0 body ...)
     (let loop ([id (node-link start axis polarity)])
       (when (not (eq? start id))
	 (let ([next (node-link id axis polarity)])
	   body0 body ...
	   (loop next))))]))

;;; Knuth's column cover algorithm.  Expects to be passed a column
;;; header.
(define (cover-column column)
  ;(printf "Cover: ~A~%" column)
  (node-remove! column 'horizontal)
  (for-each-node (row column 'vertical 'next)
    ;(printf "  row: ~A~%" (ncell-row row))
    (for-each-node (col row 'horizontal 'next)
      (node-remove! col 'vertical)
      (ncolumn-adjust-size! (ncell-column col) -1))))

(define (uncover-column column)
  (for-each-node (row column 'vertical 'prior)
    (for-each-node (col row 'horizontal 'prior)
      (ncolumn-adjust-size! (ncell-column col) 1)
      (node-reinsert! col 'vertical)))
  (node-reinsert! column 'horizontal))

;;; Solve the given DLX system, calling take answer with the list of
;;; rows that satisfies.
(define (solve-work root take-answer)
  (define (solve answer)
    (define col (find-smallest-column root))
    (if col
      (begin
	(cover-column col)
	(for-each-node (row col 'vertical 'next)
	  (for-each-node (cell row 'horizontal 'next)
	    (cover-column (ncell-column cell)))
	  (solve (cons (ncell-row row) answer))
	  (for-each-node (cell row 'horizontal 'prior)
	    (uncover-column (ncell-column cell))))
	(uncover-column col))
      (take-answer answer)))
  (solve '()))

;;; Decode a column into a nice list of the rows contained in it.
(define (decode-column head)
  (let loop ([cell (node-down head)]
	     [rows '()])
    (if (ncolumn? cell)
      (list* (ncolumn-name head) (reverse rows))
      (loop (node-down cell) (cons (ncell-row cell) rows)))))

;;; Only valid during building.
(define (show-rows root)
  (let loop ([row (node-down root)])
    (when (nrow? row)
      (cat (nrow-row row) 3 #t)
      (display ":")
      (let iloop ([cell (node-right row)])
	(when (ncell? cell)
	  (display " ")
	  (display (ncolumn-name (ncell-column cell)))
	  (iloop (node-right cell))))
      (newline)
      (loop (node-down row)))))

;;; Valid at any time.
(define (show-cols root)
  (let loop ([col (node-right root)])
    (when (ncolumn? col)
      (display (decode-column col))
      (newline)
      (loop (node-right col)))))

(define-syntax define-builder
  (syntax-rules ()
    [(_ def-name constructor field-names ...)
     (define (def-name field-names ...)
       (define n (constructor #f #f #f #f field-names ...))
       (set-cyclic! n)
       n)]))

(define-builder create-node make-node)
(define-builder create-column make-ncolumn name size)
(define-builder create-row make-nrow row)
(define-builder create-cell make-ncell row column)

;;; Set all of the pointers of this node to be cyclic.
(define (set-cyclic! node)
  (set-node-up! node node)
  (set-node-down! node node)
  (set-node-left! node node)
  (set-node-right! node node))

;;; Fresh node insertion.  Insert the new node in along axis using the
;;; given polarity.
(define (node-insert! base new axis polarity)
  (define inv-polarity (flip-polarity polarity))
  (define other (node-link base axis polarity))
  (set-node-link! new axis inv-polarity base)
  (set-node-link! new axis polarity other)
  (set-node-link! base axis polarity new)
  (set-node-link! other axis inv-polarity new))

;;; Remove the given node from the cycle.  Provided it is re-inserted
;;; in the same place, and not modified in between, it can be
;;; re-inserted with node-reinsert!.
(define (node-remove! node axis)
  (set-node-link! (node-link node axis 'prior) axis 'next
		  (node-link node axis 'next))
  (set-node-link! (node-link node axis 'next) axis 'prior
		  (node-link node axis 'prior)))

(define (node-reinsert! node axis)
  (set-node-link! (node-link node axis 'prior) axis 'next node)
  (set-node-link! (node-link node axis 'next) axis 'prior node))

;;; Validators.
;;; Check that a horizontal cycle exists.  Note that this might not terminate
;;; if there is a cycle not including the original node.
(define (ensure-cycle node axis)
  (ensure-links node axis)
  (let loop ([n2 (node-link node axis 'next)])
    (unless (eq? node n2)
      (ensure-links node axis)
      (loop (node-link node axis 'next)))))

(define (ensure-links node axis)
  (assert eq? (node-link (node-link node axis 'prior) axis 'next) node)
  (assert eq? (node-link (node-link node axis 'next) axis 'prior) node))

(define root (create-node))
