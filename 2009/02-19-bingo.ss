#lang scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Bingo is a children’s game of chance, sometimes played by adults for
;;; fun or money. Each player has a card of numbers arranged in a
;;; five-by-five grid with five randomly-chosen numbers from 1 to 15 in the
;;; first column, from 16 to 30 in the second column, 31 to 45 in the third
;;; column, 46 to 60 in the fourth column, and 61 to 75 in the fifth
;;; column; the central space is “free” and is considered to be occupied.
;;; Then a caller randomly calls numbers from 1 to 75 without replacement,
;;; each player marking the corresponding number, if it is present on their
;;; card, as occupied. The first player to have five occupied numbers in a
;;; row horizontally, in a column vertically, or along either of the two
;;; major diagonals is the winner.
;;; 
;;; What is the average number of calls required before a single card
;;; achieves bingo? In a large game with five hundred cards in play, what
;;; is the average number of calls required before any card achieves bingo?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; To make things simpler with 0-based arrays, the numbers will be
;;; 0-74, and the columns 0-14, 15-29, 30-44, 45-59, 60-74.
;;;
;;; Number the card as follows:
;;;   0  5 10 15 20
;;;   1  6 11 16 21
;;;   2  7 xx 17 22
;;;   3  8 13 18 23
;;;   4  9 14 19 24
;;;
;;; Use a monte-carlo simulation to play the games and just count how
;;; many draws it takes to win.

;;; Permute the given vector randomly.
(define (permute! vec)
  (define len (vector-length vec))
  (for ([ind (in-range len)])
    (let* ([other (random len)]
	   [tmp (vector-ref vec ind)])
      (vector-set! vec ind (vector-ref vec other))
      (vector-set! vec other tmp)))
  vec)

;;; Return a list of numbers, in the given range, randomly permuted.
(define (make-permuted-list start end)
  (define len (- end start))
  (define vec (build-vector len (λ (x) (+ start x))))
  (vector->list (permute! vec)))

;;; Generates a bingo card.  Returns a vector of length 75 listing the
;;; fields that should be set when that number is called.
(define (make-card-lookup)
  (define columns (for/list ([i (in-range 5)])
		    (take (make-permuted-list (* 15 i) (* 15 (add1 i))) 5)))
  (define cells (list->vector (apply append columns)))
  (define lookup (make-vector 75 #f))
  (for ([i (in-range 25)])
    (vector-set! lookup (vector-ref cells i) i))
  lookup)

;;; Make a blank card.
(define (make-card)
  (make-vector 25 #f))

(define wins
  '((0 1 2 3 4)
    (5 6 7 8 9)
    (10 11 13 14)
    (15 16 17 18 19)
    (20 21 22 23 24)
    (0 5 10 15 20)
    (1 6 11 16 21)
    (2 7 17 22)
    (3 8 13 18 23)
    (4 9 14 19 24)
    (0 6 18 24)
    (4 8 16 20)))

;;; Has this card won?
(define (winning? card)
  (for/or ([section (in-list wins)])
    (for/and ([cell (in-list section)])
      (vector-ref card cell))))

;;; Play this hand with a random card, and return how many pieces had
;;; to be played before winning.
(define (play-hand seq)
  (define lookup (make-card-lookup))
  (define hand (make-card))
  (let loop ([count 0]
	     [seq seq])
    (if (winning? hand) count
      (let ([index (vector-ref lookup (car seq))])
	(when index
	  (vector-set! hand index #t))
	(loop (add1 count) (cdr seq))))))

;;; Average out the single card result.
(define (single limit)
  (let loop ([count 0]
	     [total 0])
    (if (= count limit) (exact->inexact (/ total limit))
      (loop (add1 count) (+ total (play-hand (make-permuted-list 0 75)))))))

;;; Average number, for multiple hands played.
(define (multi-hands hands limit)
  (let loop ([count 0]
	     [total 0])
    (if (= count limit) (exact->inexact (/ total limit))
      (let* ([seq (make-permuted-list 0 75)]
	     [first-win (for/fold ([fw 100])
			  ([i (in-range hands)])
			  (min fw (play-hand seq)))])
	(loop (add1 count) (+ total first-win))))))
