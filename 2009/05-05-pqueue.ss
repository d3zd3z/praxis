#lang scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A priority queue is a data structure in which items arrive randomly and
;;; depart according to an ordering predicate. It is similar to a normal
;;; queue, in which items depart in the order they arrive (first-in,
;;; first-out), and a stack, in which items depart in the opposite of the
;;; order in which they arrive (last-in, first-out). The operations on
;;; priority queues include insert to add a new item to the queue,
;;; find-first to return the first item in the queue, delete-first to
;;; return the remaining items after the first, and merge to merge two
;;; queues. Priority queues are used in simulations, where keys correspond
;;; to “event times” which must be processed in order, in job scheduling
;;; for computer systems, where more-important jobs must be performed
;;; beforeless-important jobs, and in many other applications.
;;; 
;;; There are many ways to implement priority queues. An unordered list
;;; makes it easy to insert new items, but each time an item is extracted
;;; the entire list must be scanned. An ordered list makes extraction quick
;;; but requires a scan of half the list, on average, each time an item is
;;; inserted. Binary trees give a total ordering of all the items in a
;;; priority queue, but we only need to be able to identify the first item,
;;; so they do more work than we need. We will implement priority queues
;;; using leftist heaps.
;;; 
;;; A heap is a binary tree in which each node precedes its two children in
;;; a total ordering; the ordering predicate may be less-than or
;;; greater-than, as appropriate for the particular heap. A leftist heap
;;; satisfies the additional criterion that the rank of each left node is
;;; greater than or equal to the rank of its right sibling, where the rank
;;; of a node is the length of its right spine. As a result, the right
;;; spine of any node is always the shortest path to an empty node. The
;;; name leftist heap derives from the fact that the left subtree is
;;; usually taller than the right subtree, so a drawing of a leftist heap
;;; tends to “lean” left.
;;; 
;;; The fundamental operation on leftist heaps is the merge of two leftist
;;; heaps. This is accomplished by merging their right spines in the same
;;; manner as merging two sorted lists; this preserves the heap-order
;;; property. Then the children of the nodes along that new path are
;;; swapped as necessary to preserve the leftist property.
;;; 
;;; Given merge, the remaining operations are trivial. Insert builds a
;;; singleton priority queue, then merges it to the existing priority
;;; queue. Find-first simply returns the item at the root of the tree.
;;; Delete-first merges the two children of the root.
;;; 
;;; Leftist heaps were invented by Clark Crane in 1972 and popularized by
;;; Donald Knuth in 1973.
;;; 
;;; Your task is to implement the priority queue data structure using
;;; leftist heaps. When you are finished, you are welcome to read or run a
;;; suggested solution, or to post your solution or discuss the exercise in
;;; the comments below.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implement these entirely functionally.

(define-syntax assert
  (syntax-rules ()
    [(_ op a b)
     (unless (op a b)
       (error "Assertion failure" 'op 'a 'b))]))

#|
(define-syntax when
  (syntax-rules ()
    [(_ condition b0 body ...)
     (if condition (begin b0 body ...) (void))]))
(define-syntax unless
  (syntax-rules ()
    [(_ condition b0 body ...)
     (if condition (void) (begin b0 body ...))]))
(define (add1 x) (+ x 1))
(define (sub1 x) (- x 1))
|#

(define-struct node (value dist left right) #:transparent)
;(define-structure node value dist left right)

(define empty-pqueue (make-node 'empty-pqueue 0 '() '()))
(define (empty-pqueue? q)
  (eq? q empty-pqueue))

(define (pqueue-insert tree value)
  (merge tree (make-simple-node value)))

(define (pqueue-first tree)
  (if tree (node-value tree)
    (error "empty pqueue" 'pqueue-first)))
(define (pqueue-rest tree)
  (if tree (merge (node-left tree) (node-right tree))
    (error "empty pqueue" 'pqueue-rest)))

(define (safe-node-dist node)
  (node-dist node))

;;; Ensure that the dist field accurately represents the tree at hand.
(define (ensure-distance node)
  (unless (empty-pqueue? node)
    (let ([left (node-left node)]
	  [right (node-right node)])
      (ensure-distance left)
      (ensure-distance right)
      #;
      (printf "~a: ~a ~a ~a~%"
	      (node-value node)
	      (node-dist node)
	      (safe-node-dist left)
	      (safe-node-dist right))
      (assert = (node-dist node)
	      (add1 (min (safe-node-dist left) (safe-node-dist right)))))))

(define (ensure-tree node)
  (unless (empty-pqueue? node)
    (ensure-distance node)
    (let ([left (node-left node)]
	  [right (node-right node)])
      (ensure-tree left)
      (ensure-tree right)
      (unless (empty-pqueue? left)
	(assert < (node-value node) (node-value left)))
      (unless (empty-pqueue? right)
	(assert < (node-value node) (node-value right)))
      (assert >= (safe-node-dist left) (safe-node-dist right)))))

(define (make-simple-node value)
  (make-node value 1 empty-pqueue empty-pqueue))

(define (show node)
  (define (ishow node depth)
     (unless (empty-pqueue? node)
       (show (node-right node) (add1 depth))
       (spaces (* 3 depth))
       (printf "~a(~a)~%" (node-value node) (node-dist node))
       (show (node-left node) (add1 depth))))
  (ishow node 0))

(define (spaces n)
  (display (make-string n #\space)))

;;; This comes from an imperative Java implementation off of
;;; Wikipedia, however it is functional.
(define (merge a b)
  (define (mm a b)
    (let ([right (merge (node-right a) b)]
	  [left (node-left a)])
      (if left
	(if (> (node-dist left) (node-dist right))
	  (make-node (node-value a) (add1 (node-dist right)) left right)
	  (make-node (node-value a) (add1 (node-dist left)) right left))
	(make-node (node-value a) 1 right empty-pqueue))))
  (cond
    [(empty-pqueue? a) b]
    [(empty-pqueue? b) a]
    [(> (node-value a) (node-value b))
     (mm b a)]
    [else
      (mm a b)]))

(define (build seq)
  (let loop ([seq seq]
	     [pq empty-pqueue])
    (if (pair? seq)
      (loop (cdr seq)
	    (pqueue-insert pq (car seq)))
      pq)))
;  (for/fold ([pq empty-pqueue])
;    ([item seq])
;    (pqueue-insert pq item)))

;;; Verify that the queue is ordered as we remove from it.  The queue
;;; should not contain negative values.
(define (check-queue queue)
  (let loop ([last -1]
	     [queue queue])
    (unless (empty-pqueue? queue)
      (let ([hd (pqueue-first queue)]
	    [tl (pqueue-rest queue)])
	(assert > hd last)
	(loop hd tl)))))

(define (build-and-check seq)
  (define queue (build seq))
  (ensure-tree queue)
  (check-queue queue))

(define (make-park-miller seed)
  (lambda ()
    (set! seed (remainder (* seed 16807) #x7fffffff))
    seed))

(define (randoms n)
  (define v (make-vector n #f))
  (define rnd (make-park-miller 1))
  (let loop ([pos 0])
    (when (< pos n)
      (vector-set! v pos pos)
      (loop (add1 pos))))
  (let loop ([pos 0])
    (when (< pos n)
      (let* ([other (remainder (rnd) n)]
	     [tmp (vector-ref v pos)])
	(vector-set! v pos (vector-ref v other))
	(vector-set! v other tmp))
      (loop (add1 pos))))
  (vector->list v))

;  (sort (build-list n (λ (x) x))
;	(λ (a b) (zero? (random 2)))))
(define (n v) (make-simple-node v))

(time (build-and-check (randoms 100000)))
