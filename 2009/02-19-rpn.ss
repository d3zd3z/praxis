#lang scheme
;;; Implement an RPN calculator that takes an expression like 19 2.14
;;; + 4.5 2 4.3 / - * which is usually expressed as (19 + 2.14) * (4.5
;;; - 2 / 4.3) and responds with 85.2974. The program should read
;;; expressions from standard input and print the top of the stack to
;;; standard output when a newline is encountered. The program should
;;; retain the state of the operand stack between expressions.

(define stack '())

(define (push n)
  (set! stack (cons n stack)))

(define (pop)
  (match stack
    ['() (error "Empty stack")]
    [(cons hd tl)
     (set! stack tl)
     hd]))

(define (lookup-op op)
  (case op
    [(+) +]
    [(-) -]
    [(*) *]
    [(/) /]
    [else #f]))

(define (perform tok)
  (cond
    [(number? tok) (push tok)]
    [(lookup-op tok) => (λ (op)
			   (let ([b (pop)]
				 [a (pop)])
			     (push (op a b))))]
    [else (error "Unknown op" tok)]))

;;; Convert the line of tokens into a list of tokens.
(define (decode-line line)
  (define inp (open-input-string line))
  (let loop ([tokens '()])
    (let ([tok (read inp)])
      (if (eof-object? tok)
	(reverse tokens)
	(loop (cons tok tokens))))))

(define (calc)
  (define line (read-line))
  (unless (eof-object? line)
    (for-each perform (decode-line line))
    (display (reverse stack))
    (calc)))
