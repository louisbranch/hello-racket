#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride) 
  (define (seq n lst) 
    [if (<= n high) (seq (+ n stride) (cons n lst)) lst]
  )
  (reverse (seq low null))
)

(define (string-append-map lst suffix) 
  (map (lambda (word) (string-append word suffix)) lst)
)

(define (list-nth-mod xs n)
  (define (rem) (remainder n (length xs)))
  (define (nth i) (car (list-tail xs i)))
  (cond
    [(< n 0) (error "list-nth-mod: negative number")]
    [(null? xs) (error "list-nth-mod: empty list")]
    [#t (nth (rem))]
  )
)

(define (stream-for-n-steps s n) 
  (define (n-times s acc) 
    (cond
      [(= (length acc) n) acc]
      [#t 
       (let ((stream (s))) 
         (n-times (cdr (s)) (cons (car (s)) acc)))
      ]
    )
  )
  (reverse (n-times s null))
)

(define (funny-number-stream) 
  (define (stream x)
    (define next [if (= (remainder x 5) 0) (- x) x])
    (cons next (lambda () (stream (+ x 1))))
  )
  (stream 1)
)

(define (dan-then-dog)
  (define (dan) (cons "dan.jpg" dog))
  (define (dog) (cons "dog.jpg" dan))
  (dan)
)

(define (stream-add-zero s)
  (define (stream)
    (define result (s))
    (cons (cons 0 (car result)) (stream-add-zero (cdr result)))
  )
  stream
)

(define (cycle-lists xs ys)
  (define (stream n)
    (cons 
     (cons (list-nth-mod xs n) (list-nth-mod ys n)) 
     (lambda () (stream (+ n 1)))
    )
  )
  (lambda () (stream 0))
)

(define (vector-assoc v vec)
  (define (find vec i)
    (if (= (vector-length vec) i)
      #f 
      (let
        ((p (vector-ref vec i)))
        (cond
          [(and (pair? p) (equal? (car p) v)) p]
          [#t (find vec (+ i 1))]
        )
      )
    )
  )
  (find vec 0)
)

(define (cached-assoc xs n)
  (define slot 0)
  (define (increment-slot)
    (define new-slot (+ slot 1))
    (set! slot (if (= new-slot n) 0 new-slot)))
  (define (add-to-memo ans)
    (if ans
       (begin (vector-set! memo slot ans) (increment-slot) ans)
       #f
    )
  )
  (define memo (make-vector n #f))
  (define (f v)
    (define ans (vector-assoc v memo))
    (if ans ans (add-to-memo (assoc v xs)))
  )
  f
)