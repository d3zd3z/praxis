#lang scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Over two millenia ago, Eratosthenes, who calculated the circumference
;;; of the earth, the distance to the Sun and the tilt of the Earth’s axis,
;;; developed a system of latitude and longitude, and invented the leap
;;; day, created a systematic method to enumerate the prime numbers that is
;;; still in use today. Eratosthenes was born in Cyrene (present-day
;;; Libya), lived from 276 B.C. to 194 B.C., and spent most of his life in
;;; Alexandria, Egypt, where he was the second Chief Librarian of the Great
;;; Library, succeeding Apollonius of Rhodes; he was a good friend of
;;; Archimedes.
;;; 
;;; The Sieve of Eratosthenes starts by making a list of all the numbers up
;;; to a desired maximum; we’ll illustrate the method by calculating the
;;; prime numbers through thirty:
;;; 
;;; 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27
;;; 28 29 30
;;; 
;;; Now take the first number on the list, 2, and cross off every second
;;; number:
;;; 
;;; 2 3 [DEL:4:DEL] 5 [DEL:6:DEL] 7 [DEL:8:DEL] 9 [DEL:10:DEL] 11 [DEL:12
;;; :DEL] 13 [DEL:14:DEL] 15 [DEL:16:DEL] 17 [DEL:18:DEL] 19 [DEL:20:DEL]
;;; 21 [DEL:22:DEL] 23 [DEL:24:DEL] 25 [DEL:26:DEL] 27 [DEL:28:DEL] 29
;;; [DEL:30:DEL]
;;; 
;;; (Although it may not be obvious, the number 4 is crossed off the list;
;;; in some fonts, the cross-bar of the 4 coincides with the strike-through
;;; bar.) Next, take the next number on the list that isn’t crossed off, 3,
;;; and cross off every third number; some of them have already been
;;; crossed off:
;;; 
;;; 2 3 [DEL:4:DEL] 5 [DEL:6:DEL] 7 [DEL:8:DEL] [DEL:9:DEL] [DEL:10:DEL] 11
;;; [DEL:12:DEL] 13 [DEL:14:DEL] [DEL:15:DEL] [DEL:16:DEL] 17 [DEL:18:DEL]
;;; 19 [DEL:20:DEL] [DEL:21:DEL] [DEL:22:DEL] 23 [DEL:24:DEL] 25 [DEL:26
;;; :DEL] [DEL:27:DEL] [DEL:28:DEL] 29 [DEL:30:DEL]
;;; 
;;; Repeat that last step for the next un-crossed number on the list, 5:
;;; 
;;; 2 3 [DEL:4:DEL] 5 [DEL:6:DEL] 7 [DEL:8:DEL] [DEL:9:DEL] [DEL:10:DEL] 11
;;; [DEL:12:DEL] 13 [DEL:14:DEL] [DEL:15:DEL] [DEL:16:DEL] 17 [DEL:18:DEL]
;;; 19 [DEL:20:DEL] [DEL:21:DEL] [DEL:22:DEL] 23 [DEL:24:DEL] [DEL:25:DEL]
;;; [DEL:26:DEL] [DEL:27:DEL] [DEL:28:DEL] 29 [DEL:30:DEL]
;;; 
;;; And so on, each time crossing off all multiples of the next un-crossed
;;; number on the list. The list of prime numbers are all those that
;;; haven’t been crossed off:
;;; 
;;; 2 3 5 7 11 13 17 19 23 29
;;; 
;;; This method is called a sieve because it sweeps through a range of
;;; numbers, with each prime number, as it is discovered, blocking all its
;;; multiples from falling through as prime numbers. The sieve admits
;;; several optimizations. First, only odd numbers are considered, since
;;; the initial sifting crosses off all the even numbers except 2, which is
;;; handled separately. Second, crossing off starts at the square of the
;;; number being sifted, since all smaller primes have already been crossed
;;; off by previous steps of the sieve; for instance, sifting by 3 starts
;;; at 9, since 6 was already crossed off when sifting by 2. Third, sifting
;;; stops at the square root of the maximum number in the sieve, since any
;;; non-primes larger than the square root must have already been crossed
;;; off at previous levels of the sieve; thus, in the above example there
;;; is no need to sieve on the prime number 7, or any larger prime number,
;;; since the square of 7 is greater than 30, which is the largest number
;;; in the list.
;;; 
;;; Write a function that takes a single argument n and returns a list of
;;; prime numbers less than or equal to n using the optimized sieving
;;; algorithm described above. Apply the function to the argument 15485863
;;; and count the number of primes returned.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; We're going to try and be a bit efficient about this.  Store the
;;; odd numbers as bits.

(require srfi/60)
(require srfi/4)

;;; The array starts out as all ones, with numbers removed.
(define (make-sieve-vector n)
  (define size (ceiling (/ (add1 n) 64)))
  (make-u32vector size #xffffffff))

(define (prime? vec n)
  (define-values (index bit) (quotient/remainder (quotient n 2) 32))
  (not (zero? (logand (u32vector-ref vec index) (ash 1 bit)))))

(define (mark-not-prime vec n)
  (define-values (index bit) (quotient/remainder (quotient n 2) 32))
  (u32vector-set! vec index
		  (logand (u32vector-ref vec index)
			  (lognot (ash 1 bit)))))

(define (sieve limitm1)
  (define limit (add1 limitm1))
  (define vec (make-sieve-vector limit))
  (mark-not-prime vec 1)
  (for ([num (in-range 3 (add1 (integer-sqrt limit)) 2)])
    (when (prime? vec num)
      (for ([x (in-range (sqr num) limit num)]
	    #:when (odd? x))
	(mark-not-prime vec x))))
  (cons 2
	(for/list ([num (in-range 3 limit 2)]
		   #:when (prime? vec num))
	  num)))

(length (sieve 15485863))
