#lang racket
#|
            #####################################################
            ###  PLEASE DO NOT DISTRIBUTE SOLUTIONS PUBLICLY  ###
            #####################################################
|#
(provide (all-defined-out))
;; ^^^^^ DO NOT CHANGE ANY CODE ABOVE THIS LINE ^^^^^

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 1
(struct pair (left right) #:transparent)

;; Exercise 1.a
(define (pair-set-left p l) 
  (match p
    [(pair _ r) (pair l r)]
    )
)

;; Exercise 1.b
(define (pair-set-right p r) 
  (match p
    [(pair l _) (pair l r)]
    )
)

;; Exercise 1.c
(define (pair-swap p) 
  (match p
    [(pair l r) (pair r l)]
    )
)

;; Exercise 1.d
;; You can only use match* one time. You cannot use match.
(define (pair-add p1 p2) 
  (match* (p1 p2)
    [((pair l1 r1) (pair l2 r2))
      (pair (+ l1 l2) (+ r1 r2))
    ]
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 2.a
(define (name first last) 
  (lambda (x)
    (cond
      [(equal? x 'first) first]
      [(equal? x 'last) last]
    )
  )
)

;; Exercise 2.b
(define (first-name p) 
  (p 'first)
)

;; Exercise 2.c
(define (last-name p) 
  (p 'last)
)

;; Exercise 2.d
(define (full-name p) 
  (string-append (first-name p) " " (last-name p))
)

;; Exercise 2.e
(define (initials p) 
  (string-append
    (substring (first-name p) 0 1)
    (substring (last-name p) 0 1)
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 3
(define (from-with f n l)
  (match l
    ['() n]
    [(cons h t) (from-with f(f n h) t)]
  )
)

(define (max-from n l) 
  (from-with max n l)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 4
(define (min-from n l) 
  (from-with min n l)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 5: revisit Exercise 3 and Exercise 4

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 6
(define (count l) 
  (match l
    ['() 0]
    [(cons _ t) (+ 1 (count t))]
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 7
(define (sum l) 
  (match l
    ['() 0]
    [(cons h t) (+ h (sum t))]
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 8
(define (occurrences x l) 
  (match l
    ['() 0]
    [(cons h t)
      (+ (if (equal? x h) 1 0)
        (occurrences x t)
      )
    ]
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exercise 9
(define (norm l)
  (sqrt
    (sum
      (map
        (lambda (x) (* x x)) l)
    )
  )
)

