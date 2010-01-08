#lang scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This exercise comes originally from the Sphere Online Judge; I read it
;;; on Proggit based on a blog posting by Chethan T. So many of the answers
;;; were wrong that I decided it would make a good exercise for Programming
;;; Praxis. Here is SPOJ’s statement of the exercise:
;;; 
;;;     A positive integer is called a palindrome if its representation in
;;;     the decimal system is the same when read from left to right and
;;;     from right to left. For a given positive integer K of not more than
;;;     1000000 digits, write the value of the smallest palindrome larger
;;;     than K to output. Numbers are always displayed without leading
;;;     zeros.
;;; 
;;; Your task is to write a function that calculates the next palindrome
;;; larger than its input. When you are finished, you are welcome to read
;;; or run a suggested solution, or post your own solution or discuss the
;;; exercise in the comments below.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; First, make sure we have a sanity test, and implement something
;;; that works for small numbers, as well as a string to verify the
;;; results.

;;; Is this string a palindrome?
(define (palindrome? text)
  (let loop ([a 0]
	     [b (sub1 (string-length text))])
    (cond
      [(>= a b) #t]
      [(char=? (string-ref text a) (string-ref text b))
       (loop (add1 a) (sub1 b))]
      [else #f])))

(define (small-next-palindrome number)
  (let loop ([n (add1 number)])
    (if (palindrome? (number->string n)) n
      (loop (add1 n)))))

(define (string-reverse text)
  (define len (string-length text))
  (define lenm1 (sub1 len))
  (build-string len (λ (i) (string-ref text (- lenm1 i)))))

;;; Obviously, we don't have time to iterate to the next solution, so
;;; we have to compute the next answer.  There are three cases, if the
;;; first half of the number is greater than the second half, the next
;;; palindrome will be the first half repeated, otherwise, it will be
;;; the first half incremented, and repeated.  The special case is a
;;; number of all 9 digits, which we just special case, since the
;;; result has more digits, and is always just two greater than the
;;; input.
;;; These numbers are quite large, and although we don't do a lot of
;;; math with the numbers, there is some math, so bignums need to be
;;; implemented.
(define (next-palindrome number)
  (define text (number->string number))
  (define text-length (string-length text))
  (define-values (half-floor half-remainder) (quotient/remainder text-length 2))
  (define half-width (+ half-floor half-remainder))
  (define left (substring text 0 half-width))
  (define right (string-reverse (substring text (- text-length half-width) text-length)))
  (define (join text)
    (string->number
      (string-append text (string-reverse (substring text 0 half-floor)))))
  (cond
    [(regexp-match? #rx"^9+$" text) (+ number 2)]
    [(string>? left right)
     (join left)]
    [else
      (let ([new-left (number->string (add1 (string->number left)))])
	(join new-left))]))

(define dastardly
  (* (sub1 (expt 10 500000)) (expt 10 500000)))
