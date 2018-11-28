#lang racket

; IMPORTS
(require test-engine/racket-tests 2htdp/planetcute 2htdp/image)

; Number stream for testing
(define nats (letrec ([f (lambda (x)
                        (cons x
                              (lambda () (f (+ x 1)))))])
               (lambda () (f 1))))

; ---------------------------------------------------------------------------------

; 1)
; Given a step, high, and low create a list inbetween high and low iterated by step.
(define (downseries step high low)
   (if (>= high low)
       (append (list high) (downseries step (- high step) low))
       (list)))

; 2)
; Given a list of strings, returns the list with "meow" appended to each string in the list.
(define (meow-string-map strs)
  (map (lambda (x)
         (string-append x "meow"))
       strs))

; 3)
; Given a list and a number n, return the the elment i where i is list length / n. Error in invalid cases (empty list, negative n).
(define (list-ref-div lst n)
  (cond
    [(negative? n) (error "Negative n")]
    [(empty? lst) (error "Empty list")]
    [else (list-ref lst (round (/ (length lst) n)))]
    ))

; 4)
; Given a stream and a number n, returns the list of k elements from the stream
(define (next-k-items lst n)
  (if (= 0 n) (list)
  (append (list (car (lst))) (next-k-items (cdr (lst)) (- n 1)))))

; 5)
; Given a stream and a number n, returns the number at index k
(define (kth-item lst n)
  (if (= 1 n) (car (lst))
      (kth-item (cdr (lst)) (- n 1))))

; 6)
; Create a stream of natural numbers where negate all numbers present in that stream divisible by 2 and 5
(define (negate-2-and-5)
  (letrec ([f (lambda (x)
                (cons x (lambda () (f (if (or (zero? (modulo (+ 1 (abs x)) 2)) (zero? (modulo (+ 1 (abs x)) 5))) (- 0 (+ 1 (abs x)))
                                           (+ 1 (abs x)))))))])
    (lambda () (f 1))))

; 7)
; Generates a stream of key, heart, and yellow-star w/ wraparound
(define key-heart-star
  (letrec ([f (lambda (x) (cons x (lambda () (f (cond
                                                  [(equal? key x) heart]
                                                  [(equal? heart x) yellow-star]
                                                  [else key])
                                                  ))))])
    (lambda () (f key))))

; 8)
; Given a stream converts each element into a pair with first being 2
(define (two-pairs-stream s)
  (letrec ([f (lambda (x)
                (cons (cons 2 (car (x))) (lambda () (f (cdr (x))))))])
     (lambda () (f s))))

; 9)
; Given two streams, create a new stream by pulling elements whos index matches from the two into a pair repeating after they end
(define (spin-stream x y)
  (define (iter xs ys index)
      (define x-index (modulo index (length xs)))
      (define y-index (modulo index (length ys)))
      
      (cons (list-ref xs x-index) (list-ref ys y-index))
    )

  (letrec ([f (lambda (l1 l2 i) (cons (iter l1 l2 i) (lambda () (f l1 l2 (add1 i)))))])
    (lambda () (f x y 0))))

; 10)
; Takes a value and a vector, if the vector has a field with a car equal to v, return the pair otherwise false
(define (kvpv-lookup val arg)
 (letrec ([ v (vector-filter (lambda (x) (= (car x) val)) arg)])
   (if (empty? v) (#f)
   (vector-ref v 0))))

; 11
; Given a list of key-value pairs and a number n, create a cache of size n for the list with round-robin replacement
(define (cached-lookup lst n)
  
  (define cache (make-vector n #f))
  (define index 0)
  
  (define (lookup vec i)
    (if (or (negative? i) (< i (- index n))) #f
        (let ((cur (vector-ref cache (modulo i n))))
          (if (equal? (car cur) vec) (cons #t cur)
              (lookup vec (sub1 i))))))
  
  (define (insert item)
    (vector-set! cache (modulo index n) item)
    (set! index (add1 index))
    (cons #f item))
  
  (lambda (k l)
    (cond [(lookup k (sub1 index))]
          [(assoc k l) => insert]
          [else #f])))


               