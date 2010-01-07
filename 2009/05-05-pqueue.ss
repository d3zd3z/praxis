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

(define-struct node (value dist left right) #:transparent)

(define empty-pqueue #f)
(define (pqueue-insert tree value)
  (merge tree (make-simple-node value)))
(define (pqueue-take tree)
  (values (node-value tree)
	  (merge (node-left tree) (node-right tree))))

(define (safe-node-dist node)
  (if node (node-dist node)
    0))

;;; Ensure that the dist field accurately represents the tree at hand.
(define (ensure-distance node)
  (when node
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
  (when node
    (ensure-distance node)
    (let ([left (node-left node)]
	  [right (node-right node)])
      (ensure-tree left)
      (ensure-tree right)
      (when left
	(assert < (node-value node) (node-value left)))
      (when right
	(assert < (node-value node) (node-value right)))
      (assert >= (safe-node-dist left) (safe-node-dist right)))))

(define (make-simple-node value)
  (make-node value 1 #f #f))

(define show
  (case-lambda
    [(node) (show node 0)]
    [(node depth)
     (when node
       (show (node-right node) (add1 depth))
       (spaces (* 3 depth))
       (printf "~a(~a)~%" (node-value node) (node-dist node))
       (show (node-left node) (add1 depth))
       )]))

(define (spaces n)
  (display (make-string n #\space)))

;;; This comes from an imperative Java implementation off of
;;; Wikipedia, however it is functional.
(define (merge a b)
  (cond
    [(not a) b]
    [(not b) a]
    [(> (node-value a) (node-value b))
     (merge b a)]
    [else
      (let ([new-right (merge (node-right a) b)]
	    [left (node-left a)])
	(if left
	  (let-values ([(nleft nright)
			(if (> (node-dist left) (node-dist new-right))
			  (values left new-right)
			  (values new-right left))])
	    (make-node (node-value a) (add1 (node-dist nright)) nleft nright))
	  (make-node (node-value a) 1 new-right #f)))]))

(define (build seq)
  (for/fold ([pq empty-pqueue])
    ([item seq])
    (pqueue-insert pq item)))

(define (randoms n)
  (sort (build-list n (λ (x) x))
	(λ (a b) (zero? (random 2)))))
(define (n v) (make-simple-node v))
