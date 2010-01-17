#lang scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; $7.11
;;; 
;;; November 27, 2009
;;; 
;;; A mathematician purchased four items in a grocery store. He noticed
;;; that when he added the prices of the four items, the sum came to $7.11,
;;; and when he multiplied the prices of the four items, the product came
;;; to $7.11.
;;; 
;;; Your task is to determine the prices of the four items. When you are
;;; finished, you are welcome to read or run a suggested solution, or to
;;; post your solution or discuss the exercise in the comments below.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; From (require (planet soegaard/math/math)) (factorize 711000000),
;;; we learn that the factors of this value are 2^6, 3^2, 5^6, 79.
;;; The answer will be the grouping of factors, whose sum is 711.

(require scheme/control)
(require (planet soegaard/math/math))

;;; Call 'proc' on each of the divisors of the given number.  The
;;; number is given as a list of two-element lists in the form
;;; returned by by 'factorize', e.g, for 450, it would be '((2 1) (3
;;; 2) (5 2)).  'proc' will be called with the divisor as the first
;;; argument, and the remaining factors as the second argument.

(define (iterate-divisors proc factors)
  (let loop ([divs factors]
	     [unused-divs '()]
	     [prod 1])
    (match divs
      ['()
       (proc prod unused-divs)]
      [(cons (list factor count) tl)
       (for ([power (in-range (add1 count))])
	 (loop tl
	       (if (= power count) unused-divs
		 (cons (list factor (- count power)) unused-divs))
	       (* prod (expt factor power))))])))

(define (expand-factors factors)
  (let loop ([prod 1]
	     [facts factors])
    (if (null? facts) prod
      (loop (* prod (expt (caar facts) (cadar facts)))
	    (cdr facts)))))

;;; Bleh, this is complicated.
(define (compute proc answers total-sum total-product)
  (define factors (factorize total-product))
  (define ((next sofar) divisor remaining)
    ;; If there's only one left, then the only possibility is the
    ;; answer made up of those values.
    (if (= (+ (length sofar) 2) answers)
      (let* ([last-number (expand-factors remaining)]
	     [full-answer (list* last-number divisor sofar)])
	;; Show when the sum is correct, and the new value is strictly
	;; >= the previous factors.  This doesn't eliminate redundant
	;; answers if two items have the same price.
	(when (and (= total-sum (sum full-answer))
		   (or (null? sofar)
		       (>= divisor (car sofar)))
		   (>= last-number divisor))
	  (proc full-answer)))
      (when (or (null? sofar)
		(>= divisor (car sofar)))
	(iterate-divisors (next (cons divisor sofar)) remaining))))
  (iterate-divisors (next '()) factors))

(define (solve)
  (prompt
    (compute (lambda (numbers)
	       (printf "Answer: ~s~n" numbers))
	       ;(abort (void)))
	     4 711 711000000)))

(solve)
