;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; From the Jargon File:
;;; 
;;;     rot13 /rot ther’teen/ /n.,v./ [Usenet: from `rotate alphabet 13
;;;     places'] The simple Caesar-cypher encryption that replaces each
;;;     English letter with the one 13 places forward or back along the
;;;     alphabet, so that “The butler did it!” becomes “Gur ohgyre qvq vg!”
;;;     Most Usenet news reading and posting programs include a rot13 feature.
;;;     It is used to enclose the text in a sealed wrapper that the reader
;;;     must choose to open — e.g., for posting things that might offend some
;;;     readers, or spoilers. A major advantage of rot13 over rot(N) for other
;;;     N is that it is self-inverse, so the same code can be used for
;;;     encoding and decoding.
;;; 
;;; Write a function that takes a string and returns the ROT13 version of the
;;; string; you may assume that the character set is ascii. What is the
;;; meaning of “Cebtenzzvat Cenkvf vf sha!”
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang scheme

(define (char-add ch num)
  (integer->char (+ (char->integer ch) num)))

(define (rot13-char ch)
  (let ([lowered (char-downcase ch)])
    (cond
      [(char<=? #\a lowered #\m)
       (char-add ch 13)]
      [(char<=? #\n lowered #\z)
       (char-add ch -13)]
      [else ch])))

(define (rot13 str)
  (build-string (string-length str)
		(lambda (i) (rot13-char (string-ref str i)))))
