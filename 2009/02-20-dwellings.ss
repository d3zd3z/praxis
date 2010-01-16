#lang scheme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Baker, Cooper, Fletcher, Miller and Smith live on different floors of an
;;; apartment house that contains only five floors. Baker does not live on the
;;; top floor. Cooper does not live on the bottom floor. Fletcher does not
;;; live on either the top or the bottom floor. Miller lives on a higher floor
;;; than does Cooper. Smith does not live on a floor adjacent to Fletcher’s.
;;; Fletcher does not live on a floor adjacent to Cooper’s. Where does
;;; everyone live?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; There is an 'amb' available in the Swindle library.
(require swindle/extra)

(define (solve)
  (let* ([baker (amb 1 2 3 4)]
	 [cooper (amb 2 3 4 5)]
	 [fletcher (amb 2 3 4)]
	 [miller (amb 1 2 3 4 5)]
	 [smith (amb 1 2 3 4 5)])
    (amb-assert (equal? '(1 2 3 4 5) (sort (list baker cooper fletcher miller smith) <)))
    (amb-assert (> miller cooper))
    (amb-assert (not-adjacent smith fletcher))
    (amb-assert (not-adjacent fletcher cooper))
    (printf "Baker    ~a~n" baker)
    (printf "Cooper   ~a~n" cooper)
    (printf "Fletcher ~a~n" fletcher)
    (printf "Miller   ~a~n" miller)
    (printf "Smith    ~a~n" smith)))

(define (not-adjacent f1 f2)
  (> (abs (- f1 f2)) 1))
