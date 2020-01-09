;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

;; a
(define (racketlist->mupllist ori-list)
  (if (null? ori-list)
      (aunit)
      (apair (car ori-list) (racketlist->mupllist (cdr ori-list)))))
;; b
(define (mupllist->racketlist ori-list)
  (if (apair? ori-list)
      (cons (apair-e1 ori-list) (mupllist->racketlist (apair-e2 ori-list)))
      null))
      

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(int? e) e]
        [(closure? e) e]
        [(ifgreater? e)  
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)]) ;;hbsun note: we must evaluate e3 and e4 later
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(mlet? e)
         (let ([v1 (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (cons (cons (mlet-var e) v1) env)))]
        [(fun? e) (closure env e)]
        [(call? e)
         (let ([v1 (eval-under-env (call-funexp e) env)]
               [v2 (eval-under-env (call-actual e) env)])
           (if (closure? v1)
               (letrec ([v1-fun (closure-fun v1)]
                        [v1-env (closure-env v1)]
                        [final-env (if (fun-nameopt v1-fun)
                                       (append (list (cons (fun-nameopt v1-fun) v1) (cons (fun-formal v1-fun) v2)) v1-env)
                                       (append (list (cons (fun-formal v1-fun) v2)) v1-env))])
                 (eval-under-env (fun-body v1-fun) final-env))
               (error (format "MUPL CALL expressionapplied to no-closure: ~v" e))))]
        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error (format "bad FST to no-apair for (fst ~v) in ~e" v e))))]
        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error (format "bad SND to no-apair ~v" e))))]
        [(isaunit? e)
         (let ([v (eval-under-env (isaunit-e e) env)])
           (if (aunit? v)
               (int 1)
               (int 0)))]
        [(aunit? e) (aunit)]
        [#t (error (format "bad MUPL expression: ~v" e))]))


;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3 a
(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

;; Problem 3 b
;; hbsun: at the beginning, I made a mistake that reverse the order of `mlet` and `mlet*`
(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (let ([v (car lstlst)])
         (mlet (car v) (cdr v) (mlet* (cdr lstlst) e2)))))

;; Problem 3 c
;; hbsun: the assignment requires us use the name of "_x" and "_y"
(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2))
         (ifgreater (var "_x") (var "_y") e4 (ifgreater (var "_y") (var "_x") e4 e3))))

;; Problem 4 a
(define mupl-map (fun #f "apply-fun" (fun "helper-fun" "xs"
                                          (ifaunit (var "xs")
                                                   (aunit)
                                                   (apair
                                                    (call (var "apply-fun") (fst (var "xs")))
                                                    (call (var "helper-fun") (snd (var "xs"))))))))
;; Problem 4 b
(define mupl-mapAddN
  (mlet "map" mupl-map
        (fun #f "i" (call (var "map") (fun #f "x" (add (var "x") (var "i")))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
