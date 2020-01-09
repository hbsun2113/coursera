#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
;; 1
(define (sequence low high stride)
  (if (> low high)
      null
      (cons low (sequence (+ low stride) high stride))))

;;2
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

;;3
;; shorter but less readable for not using let
(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

;;4
(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([pr (s)])
        (cons (car pr) (stream-for-n-steps (cdr pr) (- n 1))))))

;;5
(define funny-number-stream
  (letrec ([f (lambda (x) (cons
                           (if(= 0 (remainder x 5))
                              (- 0 x)
                              x)
                           (lambda () (f (+ x 1)))))])
  (lambda () (f 1))))





;;6
;;there are three sample solutions for the question
;;mine is belong to the third one, but I prefer the first
(define dan-then-dog
  (letrec ([f (lambda (x) (cons
                            (if(even? x)
                               "dan.jpg"
                               "dog.jpg")
                            (lambda () (f (+ x 1)))))])
    (lambda () (f 0))))

;;7
;;test for (stream-for-n-steps (stream-add-zero twos) 12)
;(define twos
;  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 2)))))])
;  (lambda () (f 2))))
(define (stream-add-zero s)
  (lambda () ;;lazy
    (letrec ([pr (s)])
             (cons (cons 0 (car pr)) (stream-add-zero (cdr pr))))))
      

;;8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (x) (cons
                               (cons (list-nth-mod xs x) (list-nth-mod ys x))
                               (lambda () (f(+ x 1)))))])
    (lambda () (f 0))))

;;9
(define (vector-assoc v vec)
  (letrec ([f (lambda (x)
                (cond [(>= x (vector-length vec)) #f]
                      [(pair? (vector-ref vec x)) (if (equal? v (car (vector-ref vec x))) (vector-ref vec x) (f (+ x 1)))]
                      [#t (f (+ x 1))]))])
    (f 0)))

;;10
;; I wonder whether my cache is actually used, please tell me.
(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
          [index 0]
          [f (lambda (x)
               (let ([ans (vector-assoc x cache)])
                 (if ans
                 ans
                 (begin
                   (let ([res (assoc x xs)])
                   (vector-set! cache index res)
                   (if (= index (- n 1)) (set! index 0) (set! index (+ index 1)))
                   res)))))])
    f))

;;11
;; I refer the sample solution. I wonder know who is the `recursive thunk`, why it is `thunk` ?
(define-syntax while-less (syntax-rules (do)
((while-less e1 do e2)
  (let ([r1 e1])
    (letrec ([f (lambda ()
      (let ([r2 e2])
        (if (< r2 r1) (f) #t)))])
      (f))))))








      


