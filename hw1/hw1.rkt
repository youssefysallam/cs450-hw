#lang racket
#|
    ===> PLEASE DO NOT DISTRIBUTE THE SOLUTIONS PUBLICLY <===

  We ask that solutions be distributed only locally -- on paper, on a
  password-protected webpage, etc.

  Students are required to adhere to the University Policy on Academic
  Standards and Cheating, to the University Statement on Plagiarism and the
  Documentation of Written Work, and to the Code of Student Conduct as
  delineated in the catalog of Undergraduate Programs. The Code is available
  online at: http://www.umb.edu/life_on_campus/policies/code/

                    * * * ATTENTION! * * *

  Every solution submitted to our grading server is automatically compared
  against a solution database for plagiarism, which includes every solution
  from every student in past semesters.

  WE FOLLOW A ZERO-TOLERANCE POLICY: any student breaking the Code of Student
  Conduct will get an F in this course and will be reported according to
  Section II Academic Dishonesty Procedures.

|#

;; Please, do not remove this line and do not change the function names,
;; otherwise the grader will not work and your submission will get 0 points.
(provide (all-defined-out))

(define ex1 
  (+
    (+ 
      9
        (+ 13 5))
    (/ 
      4 
        (/ 2 5)
    )
  )
)
(define ex2 
  (list 
    ; Step 1
    (+ 
      (+ 
        9 
        (+ 13 5)
      ) 
      (/ 
        4 
        (/ 2 5)
      )
    )
    ; Step 2
    (+ 
      (+ 
        9 
        18
      ) 
      (/ 
        4 
      (/ 
        2 
        5
      )
      )
    )
    ; Step 3
    (+ 
      27 
      (/ 
        4 
        (/ 
          2 
          5
        )
      )
    )
    ; Step 4
    (+ 
      27 
      (/ 
      4 
      (/ 
        2 
        5
      )
        )
        )
    ; Step 5
    (+ 
      27
      10
    )
    ; Step 6
      37
  )
)
(define (ex3 x y)
  (<
    (- (* 2 3) y)
    (* (- x 7) (* x 13))
    )
    )

;; Constructs a tree from two trees and a value
(define (tree left value right) (list left value right))

;; Constructs a tree with a single node
(define (tree-leaf value) (tree '() value '()))

;; Accessors
(define (tree-left self) (first self))
(define (tree-value self) (second self))
(define (tree-right self) (third self))

;; Copies the source and updates one of the fields
(define (tree-set-value self value)
  (list (tree-left self) value (tree-right self)))

(define (tree-set-left self left)
  (list left (tree-value self) (tree-right self)))

(define (tree-set-right self right)
  (list (tree-left self) (tree-value self) right))

;; Function that inserts a value in a BST
(define (bst-insert self value)
  (cond
    ((null? self) (tree-leaf value))
    ((= value (tree-value self)) self)
    ((< value (tree-value self))
     (tree-set-left self
       (bst-insert (tree-left self) value)))
    (else
     (tree-set-right self
       (bst-insert (tree-right self) value)))))

;; lambda
(define (lambda? node) 
  (and
    (list? node)
    (not (null? node))
    (equal? (first node) 'lambda)
    (list? (second node))
    (andmap symbol? (second node))
  )
)
(define (lambda-params node) 
  (second node)
)
(define (lambda-body node)
  (rest(rest node))
)
;; apply
(define (apply? l) 
  (and 
  (list? l)
    (not (null? l))
  )
)
(define (apply-func node) 
  (first node)
)
(define (apply-args node) 
  (rest node)
)

;; define
(define (define? node) 
(or
    (define-basic? node)
    (define-func? node)
  )
)
(define (define-basic? node)
  (and
    (list? node)
    (= (length node) 3)
    (equal? 'define (first node))
    (symbol? (second node))
  )
)
(define (define-func? node) 
  (and
    (list? node)
    (>= (length node) 3)
    (equal? 'define (first node))
    (list? (second node))
    (not (null? (second node)))
    (andmap symbol? (second node))
  )
)

#|

EXERCISE 6

1. Input the following prompt in a Generative-AI tool such as Claude, ChatGPT,
   or Deepseek.
2. Paste below ANSWER the 4 test cases and review each test in terms of
   correcntesss, that is, should the test pass or fail?
3. Take a screenshot and upload your interaction to Gradescope

PROMPT:

Suppose that I am implementing function `define-basic?`. Below are some test
cases. Help me generate 4 new test cases: 2 positive tests and 2 negative tests.
```
; 4.h)
(define-test-suite test-4.h
  (test-case "Exercise 4.h"
    (check-true (define-basic? (quote (define x 3))))
    (check-false (define-basic? '(define)))
    (check-false (define-basic? '(define 1)))
    (check-false (define-basic? '(define 1 2)))
    (check-false (define-basic? '(define () 2)))
    (check-false (define-basic? '(define (1) 2)))
    (check-false (define-basic? '(define (x) 2)))
    (check-false (define-basic? '(define (x 1) 2)))
    (check-false (define-basic? '(1 define 2)))
    (check-false (define-basic? '(1 2 define)))
    (check-false (define-basic? '(define 2 define)))
    (check-true (define-basic? '(define define 2)))
    (check-true (define-basic? '(define define define)))
  )
)
```

ANSWER:

    (check-true  (define-basic? '(define y "hi")))         ; symbol name, string value
    (check-true  (define-basic? '(define x '(1 2 3))))     ; symbol name, quoted list value

    (check-false (define-basic? '(define x)))              ; missing value
    (check-false (define-basic? '(define 'x 1)))           ; invalid (quoted) identifier

|#
