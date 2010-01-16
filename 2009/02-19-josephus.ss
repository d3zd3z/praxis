#lang scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Flavius Josephus was a famous Jewish historian of the first century, at
;;; the time of the destruction of the Second Temple. According to legend,
;;; during the Jewish-Roman war he was trapped in a cave with a group of forty
;;; soldiers surrounded by Romans. Preferring death to capture, the Jews
;;; decided to form a circle and, proceeding around it, to kill every third
;;; person remaining until no one was left. Josephus found the safe spot in
;;; the circle and thus stayed alive.
;;; 
;;; Write a function josephus(n,m) that returns a list of n people, numbered
;;; from 0 to n-1, in the order in which they are executed, every m^th person
;;; in turn, with the sole survivor as the last person in the list. What is
;;; the value of josephus(41,3)? In what position did Josephus survive?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A queue begins with the elements as the car and the reverse of the
;;; added elements as the cdr.
(define empty-queue (cons '() '()))
(define (queue-empty? q)
  (and (null? (car q))
       (null? (cdr q))))

;;; Make a queue from a list of items.
(define (list->queue a) (cons a '()))

;;; Return a new queue with the given item added.
(define (queue-add q item)
  (cons (car q) (cons item (cdr q))))

;;; Return two values, the head of the queue, and a new queue with the
;;; rest of the items.
(define (queue-take q)
  (if (null? (car q))
    (let ([items (reverse (cdr q))])
      (values (car items) (cons (cdr items) '())))
    (values (caar q)
	    (cons (cdar q) (cdr q)))))

(define (make-list n)
  (let loop ([pos (sub1 n)]
	     [result '()])
    (if (negative? pos) result
      (loop (sub1 pos) (cons pos result)))))

(define (josephus men step)
  (define start (make-list men))
  (let loop ([phase 1]
	     [people (list->queue start)])
    (let-values ([(hd tl) (queue-take people)])
      (if (queue-empty? tl) hd
	(if (= phase step)
	  (loop 1 tl)
	  (loop (add1 phase) (queue-add tl hd)))))))

;;; It's not really necessary to keep the lists.  This is from the
;;; Wikipedia description.  However, it's not tail recursive, and
;;; consumes stack of size 'men'.
(define (jos2 men step)
  (if (<= men 1) 0
    (remainder (+ (jos2 (sub1 men) step) step) men)))

;;; So, the iterative solution is even better, no stack or memory
;;; allocations at all.
(define (jos3 men step)
  (let loop ([n 2]
	     [prior 0])
    (if (> n men) prior
      (loop (add1 n)
	    (remainder (+ prior step) n)))))

(require scheme/unsafe/ops)
;;; Interesting compilation test.  Using unsafe fixnum ops is
;;; unnoticeably different in performance.  This suggests that the
;;; Mzscheme compiler does a good job of eliminating extraneous fixnum
;;; checks.
(define (jos4 men step)
  (if (and (fixnum? men) (fixnum? step))
    (let loop ([n 2]
	       [prior 0])
      (if (unsafe-fx> n men) prior
	(loop (unsafe-fx+ n 1)
	      (unsafe-fxremainder (unsafe-fx+ prior step) n))))
    (jos3 men step)))

;;; For the case of 4 million, about 10s for josephus, about 1.3s for
;;; jos2, and 153ms for jos3.
