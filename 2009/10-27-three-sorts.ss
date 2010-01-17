#lang scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Three Quadratic Sorts
;;; 
;;; October 27, 2009
;;; 
;;; Sorting is one of the most common computing tasks. In the days of large
;;; mainframes, sorting would often account for ten percent of a computer’s
;;; workload, and there were complicated procedures involving large
;;; free-standing tape machines for sorting more records than could fit in
;;; the computer’s memory; the programmer who could shave a few percentage
;;; points of time or core memory space off the standard system sort was a
;;; hero. Nowadays, most programmers simply call their local sort library,
;;; and never worry about how it works.
;;; 
;;; We are going to explore classical sorting algorithms in the next
;;; several exercises. The rules of the game: We will be sorting arrays of
;;; integers with elements stored in locations zero through n−1, where n is
;;; the number of elements in the array. We will always sort into ascending
;;; order, and will use <, never ≤, to compare array elements. All sorting
;;; functions will be called with two parameters, the name of the array and
;;; its length.
;;; 
;;; Today, we will look at three simple sorting algorithms. Bubble sort
;;; works by repeatedly stepping through the array to be sorted, comparing
;;; each pair of adjacent elements and interchanging them if they are in
;;; the wrong order, until the array is sorted. Selection sort works by
;;; repeatedly passing through the array, at each pass finding the minimum
;;; element of the array, interchanging it with the first element of the
;;; array, then repeating on the sub-array that excludes the first element
;;; of the array. Insertion sort works the same way that card players
;;; generally sort their hands; starting from an empty hand, they pick up a
;;; card, insert it into the correct position, then repeat with each new
;;; card until no cards remain.
;;; 
;;; Your task is to write functions that sort an array using bubble sort,
;;; selection sort, and insertion sort; you should also write a test
;;; program that can be used for any of the sorting algorithms. When you
;;; are finished, you are welcome to read or run a suggested solution, or
;;; to post your own solution or discuss the exercise in the comments
;;; below.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Since the mentioned solution says that arrays are awkward, let's
;;; do just that.

;;; These macros easily define utilities for working with these
;;; arrays.
(define-syntax define-swap
  (syntax-rules ()
    [(_ swap! vec)
     (define (swap! a b)
       (let ([tmp (vector-ref vec a)])
	 (vector-set! vec a (vector-ref vec b))
	 (vector-set! vec b tmp)))]))

(define-syntax define-compare
  (syntax-rules ()
    [(_ cmp vec)
     (define (cmp a b)
       (< (vector-ref vec a) (vector-ref vec b)))]))

;;; First, a quick routine to scramble make a vector of scrambled
;;; elements.
(define (make-scrambled-vector len)
  (define data (build-vector len (lambda (x) x)))
  (define-swap swap! data)
  (for ([i (in-range len)])
    (swap! i (random len)))
  data)

;;; And something to tell us if a vector is sorted.
(define (sorted? vec)
  (define-compare elt< vec)
  (for/and ([i (in-range (sub1 (vector-length vec)))])
    (elt< i (add1 i))))

;;; Bubble sort.
(define (bubble-sort vec)
  (define len (vector-length vec))
  (define-swap swap! vec)
  (define-compare elt< vec)
  (let loop ([pos 0]
	     [did-swap #f])
    (let ([next (+ pos 1)])
    (if (>= next len)
      (if did-swap (loop 0 #f) vec)
      (if (elt< pos next)
	(loop next did-swap)
	(begin
	  (swap! pos next)
	  (loop next #t)))))))

;;; Selection sort
(define (selection-sort vec)
  (define len (vector-length vec))
  (define-swap swap! vec)
  (define-compare elt< vec)
  (for ([mark (in-range (sub1 len))])
    (let ([small-pos (for/fold ([min-pos mark])
		       ([pos (in-range (add1 mark) len)])
		       (if (elt< pos min-pos)
			 pos min-pos))])
      (swap! mark small-pos)))
  vec)

;;; Insertion sort.
(define (insertion-sort vec)
  (for ([mark (in-range 1 (vector-length vec))])
    (let ([value (vector-ref vec mark)])
      (do ([pos (sub1 mark) (sub1 pos)])
	((or (negative? pos) (< (vector-ref vec pos) value))
	 (vector-set! vec (add1 pos) value))
	(vector-set! vec (add1 pos) (vector-ref vec pos)))))
  vec)

(define (bench alg size)
  (define vec (make-scrambled-vector size))
  (time (alg vec))
  (unless (sorted? vec)
    (error "Sort didn't work")))
