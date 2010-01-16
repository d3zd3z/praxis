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

(require srfi/26)
(require scheme/control)

(require "dancing-links.ss")

;;; Solve the sample problem.
(define (main)
  (solve-work (make-constraints "7..1......2.....15.....639.2...18....4..9..7....75...3.785.....56.....4......1..2")
	      print-solution))

;;; And a sample "extreme" problem.
(define (main-extreme)
  (solve-work (make-constraints "1.......2.9.4...5...6...7...5.9.3.......7.......85..4.7.....6...3...9.8...2.....1")
	      print-solution))

(define (main-hardest)
  (solve-work (make-constraints "7......1946.19.......6827.4.9......7...3..4.5..67.......1......2...74......2..3..")
	      print-solution))

(define (main-wild)
  (solve-work (make-constraints "002090107038600000400000000000005000009010300000400000000000004000007920806030700")
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
