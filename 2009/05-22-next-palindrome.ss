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

;;; Is this string a palindrome?
(define (palindrome? text)
  (let loop ([a 0]
	     [b (sub1 (string-length text))])
    (cond
      [(>= a b) #t]
      [(char=? (string-ref text a) (string-ref text b))
       (loop (add1 a) (sub1 b))]
      [else #f])))

(define (next-palindrome number)
  (let loop ([n (add1 number)])
    (if (palindrome? (number->string n)) n
      (loop (add1 n)))))
