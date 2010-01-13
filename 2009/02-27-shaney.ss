#lang scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; In the mid-1980’s, Mark V. Shaney posted messages such as this to the
;;; Usenet group net.singles:
;;; 
;;;     I seem to be important. For me, it would have agreed with the
;;;     technical insight that is dear to me. Because of this, I have
;;;     no advice for someone in that situation!
;;; 
;;;     Joining Mensa was something I did him one better. I wore a
;;;     dress skirt a day for one week. I did him one better. I wore a
;;;     dress skirt a day for a 2 year relationship. I’m wondering if
;;;     anyone else out there has ever experienced this phenomena,
;;;     whether it was actually your contention that this is true for
;;;     me.
;;; 
;;;     I suppose it depends how you felt about someone before you
;;;     became emotionally attached and therefore “safer” – not to
;;;     sporting events, but to opera.
;;; 
;;;     I lost 90 lbs a few months during my “flower child” days in
;;;     high school where, due to her high academic standings, was
;;;     shunned by many of the tube. The experience really screwed
;;;     them up — if not their heads, their knees. Why does one have
;;;     to be the prime measurement of manhood. No?
;;; 
;;;     He was a scrawny, spastic nerd in high school, and I
;;;     fantasized about such a thing. It all depends on the
;;;     sidelines, listening to what makes the rest of the guys around
;;;     her – suddenly finds herself in a situation where guys are
;;;     asking them out!? But this can result in members of either the
;;;     person of your dreams (in a larger number of males to females
;;;     studying the field of engineering), the ratio of males to
;;;     females is somewhere in the past. And, per the other person.
;;; 
;;;     I find it hard to reconcile the notion that something or
;;;     someone isn’t theirs anymore. I have a date with the woman.
;;;     Subjectively, I have also acted in this weekend.
;;; 
;;; However, Shaney wasn’t a person. Shaney was a bot created by three Bell
;;; Labs researchers — Bruce Ellis, Rob Pike and Don Mitchell — that
;;; analyzed Usenet postings and then created its own posting. Shaney’s
;;; writings were quirky, non-sensical, and beloved by many.
;;; 
;;; Shaney worked by reading a training text and saving each triple of
;;; words that appear in the training text in a large table. Then it
;;; generates text using a markov chain, starting with two words that
;;; appear in the training text and repeatedly writing the third word of
;;; the triple, sliding the output window from word to word. The genius of
;;; the method is that any two words may appear in the training text with
;;; multiple following words, and the generator is free to choose any of
;;; them; thus, short fragments of text make sense, but the text as a whole
;;; frequently veers from one train of thought to another. A word includes
;;; its surrounding punctuation, so that sentence structure and,
;;; indirectly, grammar, are built in to the output.
;;; 
;;; Write a program that implements Shaney.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Handles all of the words in the input file, allowing for state to
;;; be carried through.  Calls 'proc' on the first word on the input.
;;; This procedure should then return a procedure which will be called
;;; on the second word, and so on.
(define (for-words file first-proc)
  (define (run port)
    (define (handle-line line proc)
      (unless (eof-object? line)
	(let ([words (regexp-split #px"\\s+" line)])
	  (handle-words (trim-empties words) proc))))
    (define (handle-words words proc)
      (if (pair? words)
	(let ([next-proc (proc (car words))])
	  (handle-words (cdr words) next-proc))
	(handle-line (read-line port 'any) proc)))
    (handle-words '() first-proc))
  (call-with-input-file* file run))

;;; Trim the empty strings out of the matched regexp.
(define (trim-empties words)
  (filter (lambda (word) (not (string=? word ""))) words))

(define prefix-length 2)

;;; The prefixes that we search for are represented as lists of
;;; strings.  To reduce a little of the copying, the prefix lists are
;;; kept in reverse order.
(define (append-word pfx word)
  (cons word (if (>= (length pfx) prefix-length)
	       (take pfx (sub1 prefix-length))
	       pfx)))

;;; Initially compute the prefixes as a hash of words to their counts.
(define (add-word mapping word)
  (hash-update! mapping word add1 0))

;;; Convert a hash of word to count to a cons of a total count,
;;; followed by pairs of (count . word) with all of the counts adding
;;; up to total.
(define (convert-mapping mapping)
  (let loop ([idx (hash-iterate-first mapping)]
	     [count 0]
	     [items '()])
    (if idx
      (let ([this-count (hash-iterate-value mapping idx)]
	    [this-word (hash-iterate-key mapping idx)])
	(loop (hash-iterate-next mapping idx)
	      (+ count this-count)
	      (cons (cons this-count this-word) items)))
      (cons count items))))

;;; From a converted mapping, choose the next word randomly.
(define (random-next cmapping)
  (define index (random (car cmapping)))
  (let loop ([offset 0]
	     [vals (cdr cmapping)])
    (let ([next-offset (+ offset (caar vals))])
      (if (> next-offset index)
	(cdar vals)
	(loop next-offset (cdr vals))))))

;;; Convert prefix hash to the new mapping format.
(define (convert-prefix-hash input)
  (for/hash ([(key value) (in-hash input)])
    (values key (convert-mapping value))))

;;; Pick a random key/value out of a hash, returning the two values.
(define (hash-randomly table)
  (define count (hash-count table))
  (define elt (random count))
  (let loop ([idx (hash-iterate-first table)]
	     [pos 0])
    (if (= elt pos)
      (values (hash-iterate-key table idx) (hash-iterate-value table idx))
      (loop (hash-iterate-next table idx)
	    (add1 pos)))))

(define prefix-hash (make-hash))
(define converted-hash #f)

(define (add-prefix pfx new-word)
  (hash-update! prefix-hash pfx
		(lambda (mapping)
		  (add-word mapping new-word)
		  mapping)
		make-hash))

(define (load-words file)
  (define ((handle context) word)
    (when (>= (length context) prefix-length)
      (add-prefix context word))
    (handle (append-word context word)))
  (for-words file (handle '())))

;;; After the words have been loaded generate 'n' random words,
;;; printing them out.  Note that if we reach the end of the text, and
;;; there is nothing following it, we just start over again somewhere
;;; randomly.
(define (make-words n)
  (define mapping
    (or converted-hash
	(let ([m (convert-prefix-hash prefix-hash)])
	  (set! converted-hash m)
	  m)))
  (define-values (seed seed-data) (hash-randomly mapping))
  (printf "~a ~a" (second seed) (first seed))
  (let loop ([context seed]
	     [column (+ 1 (string-length (first seed)) (string-length (second seed)))]
	     [count n])
    (when (positive? count)
      (let ([ref (hash-ref mapping context #f)])
	(if ref
	  (let* ([next-word (random-next ref)]
		 [size (string-length next-word)]
		 [new-column (if (> (+ column size) 70) 0 (+ column size))])
	    (if (zero? new-column)
	      (newline)
	      (display #\space))
	    (display next-word)
	    (loop (append-word context next-word) new-column (sub1 count)))
	  (begin
	    (newline)
	    (make-words count))))))
  (newline))
