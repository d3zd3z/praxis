#lang scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Word search puzzles are a popular time-wasting pasttime. To redeem some
;;; of that lost time, we will write a program to search puzzles. For
;;; instance, given a problem like
;;; 
;;; F Y Y H N R D
;;; R L J C I N U
;;; A A W A A H R
;;; N T K L P N E
;;; C I L F S A P
;;; E O G O T P N
;;; H P O L A N D
;;; 
;;; and the list of words ITALY, HOLLAND, POLAND, SPAIN, FRANCE, JAPAN,
;;; TOGO, and PERU, you should find words at the following locations:
;;; 
;;; ITALY row 5 column 2 up
;;; HOLLAND row 7 column 1 up right
;;; POLAND row 7 column 2 right
;;; SPAIN row 5 column 5 up
;;; FRANCE row 1 column 1 down
;;; JAPAN row 2 column 3 down right
;;; TOGO row 6 column 5 left
;;; PERU row 5 column 7 up
;;; 
;;; Your task is to write a word search solver. When you are finished, you
;;; are welcome to read or run a suggested solution, or to post your own
;;; solution or discuss the exercise in the comments below.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define board
  #("FYYHNRD"
    "RLJCINU"
    "AAWAAHR"
    "NTKLPNE"
    "CILFSAP"
    "EOGOTPN"
    "HPOLAND"))

(define height (vector-length board))
(define width (string-length (vector-ref board 0)))

;;; Get the letter on the board in a given position, or #f if the
;;; letter is off board.
(define (board-ref row col)
  (and (< -1 row height)
       (< -1 col width)
       (string-ref (vector-ref board row) col)))

;;; Does the given word match a row,col in direction dx,dy?
(define (word-match? word row col dy dx)
  (define word-length (string-length word))
  (let loop ([y row]
	     [x col]
	     [pos 0])
    (cond
      [(>= pos word-length) #t]
      [(eqv? (string-ref word pos) (board-ref y x))
       (loop (+ y dy) (+ x dx) (add1 pos))]
      [else #f])))

(define (dy-name dy)
  (vector-ref '#(" up" "" " down") (add1 dy)))
(define (dx-name dx)
  (vector-ref '#(" left" "" " right") (add1 dx)))

;;; Scan the board for a given word.
(define (scan-for word)
  (for* ([y (in-range height)]
	 [x (in-range width)]
	 [dy (in-range -1 2)]
	 [dx (in-range -1 2)]
	 #:when (not (= dx dy)))
    (when (word-match? word y x dy dx)
      (printf "~a row ~a column ~a~a~a~%"
	      word
	      (add1 y) (add1 x)
	      (dy-name dy)
	      (dx-name dx)))))

(for-each scan-for '("ITALY" "HOLLAND" "POLAND" "SPAIN" "FRANCE" "JAPAN" "TOGO" "PERU"))
