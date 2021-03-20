(define (% a n)
    (if (not (and (integer? a) (integer? n))) "impossible"
        (if (= n 0) "impossible"
            (remainder a (abs n))
    )   )
)
(define (<= a b)
    (if (< a b) #t
        (if (= a b) #t #f)
    )
)
(define (>= a b)
    (if (> a b) #t
        (if (= a b) #t #f)
    )
)
(define (abs x)
    (if (not (real? x)) "impossible"
         (if (< x 0) (- x) x)
    )
)
;;; to add a after b inside x
(define (add a b x)
    (if (not (belongs? b x)) "impossible"
        (append (append (head b x) (list b)) 
            (append (list a) (tail b x))
        )
    )
)
(define (append x y)
    (define dest '())
    (define (copy a) 
        (if (null? a) dest
            (begin
                (set! dest (enclose (car a) dest))
                (copy (cdr a))
            )
        )
    )
    (if (not (pair? x)) 
        (if (null? x) (if (pair? y) y (cons y x)) 
            (if (or (pair? y) (null? y)) (cons x y) 
                (list x y)
            )
        )
        (begin
            (copy x)
            (if (or (pair? y) (null? y)) (copy y) 
                (set! dest (enclose y dest))
            )
            dest
        )
    )
)
(define (atom? x) 
    (not (pair? x))
)
(define (belongs? a x)
    (cond
        ((null? x) #f)
        ((not (pair? x)) "impossible")
        ((= (car x) a) #t)
        (else (belongs? a (cdr x)))
    )
)
(define (cadr x)
    (if (not pair? x) "impossible"
        (car (cdr x))
    )
)
(define (cddr x)
    (if (not pair? x) "impossible"  
        (cdr (cdr x))
    )
)
(define (check-point p) 
    (and (real? (point-x p)) (real? (point-y p))
    )
)
(define (doble)
   (display "Value of x : ")
   (let ((x (read)))
      (cond 
         ((number? x) (* x 2))
         ((pair? x) (map (lambda (e) (* e 2)) x))
         (else (display "Error"))
      )
   )
)
;;; (enclose 4 '(1 2 3))  =>  (1 2 3 4)
(define (enclose a x)
    (define (join x y)
        (if (not (pair? x)) y
            (cons (car x) (join (cdr x) y))
        )
    )
    (if (not (or (pair? x) (null? x))) "impossible"
        (join x (list a))
    )
)
(define (even? n)
    (if (not (integer? n)) "impossible"
        (if (= (remainder n 2) 0) #t #f)
    )
)
;;; to compute x^n
(define (exp x n) 
    (if (not (and (number? x) (integer? n))) "impossible"
        (cond
            ((= n 0) 1)
            ((= n 1) x)
            ((= n -1) (/ 1 x))
            ((> n 0) (* x (exp x (- n 1))))
            (else (exp (/ 1 x) (- n)))
        )
    )
)
(define (fact n) 
    (if (not (integer? n)) "impossible"
        (if (< n 0) 0
            (if (= n 0) 1 
                (* (fact (- n 1)) n)
            )
        )
    )
)
;;; to compute a Fibonacci number
(define (fib n) 
    (define (fib-aux i a b) 
        (if (= n i) a 
            (fib-aux (+ i 1) (+ a b) a)
        )
    ) 
    (if (not (integer? n)) "impossible"
        (if (< n 0) 0 
            (fib-aux 0 0 1)
        )
    )
)
(define (gcd a b) 
    (define na (abs a))
    (define nb (abs b))
    (if (not (and (integer? a) (integer? b))) "impossible"
        (if (= (remainder na nb) 0) nb 
            (gcd nb (remainder na nb))
        )
    )
)
(define (head a x)
    (if (not (belongs? a x)) (list)
        (if (= a (last x)) (truncate x)
            (head a (truncate x))
        )
    )
)
(define (last x)
    (if (not pair? x) "impossible"
        (if (null? x) "empty list"
            (if (null? (cdr x)) (car x)
                (last (cdr x))
            )
        )
    )
)
(define (length x) 
    (if (not (or (pair? x) (null? x))) "impossible"
        (if (null? x) 0 
            (+ 1 (length (cdr x)))
        )
    )
)
(define (make-point x y)
    (if (not (and (real? x) (real? y))) "impossible"
        (vector 'point x y)
    )
)
;;; (map square '(5 7/3 2.1))  =>  (25 49/9 4.41)
(define (map f x)
    (if (not (or (pair? x) (null? x))) "impossible"
        (if (null? x) '()
            (cons (f (car x)) (map f (cdr x)))
        )
    )
)
(define (next-prime n)
    (define (new-prime n)
        (if (prime? n) n
            (new-prime (+ n 1))
        )
    )
    (if (not (integer? n)) "impossible"
        (if (prime? n) (new-prime (+ n 1)) 
            (new-prime n)
        )
    )
)
;;; (nth 3 '(1 2 4 8))  =>  4
(define (nth n x)
    (define (with k a)
        (if (= k 1) (car a)
            (with (- k 1) (cdr a))
        )
    )
    (if (or (not (integer? n)) (not (pair? x)) 
        (< n 1) (> n (length x))) "impossible"
        (with n x)
    )
)
(define (odd? n)
    (if (not (integer? n)) "impossible"
        (if (= (remainder n 2) 0) #f #t)
    )
)
(define (opaque-vector . args)
    (let ((vec (make-opaque-vector (length args))))
        (define (init idx rest-args)
            (if (not (pair? rest-args)) vec
                (begin 
                    (vector-set! vec idx (car rest-args))
                    (init (+ idx 1) (cdr rest-args))
                )
            )
        )
        (init 0 args)
    )
)
;;; approximation of pi to get 25 exact decimal digits
(define (pi)
    (define n 32)
    (define den (+ (+ n n) 1)) 
    (define (next k)
        (if (= k 0) den
            (begin
                (set! den (+ (- (+ k k) 1) (/ (* k k) den)))
                (next (- k 1))
            )
        )
    )
    (next n)
    (/ 4 den)
)
(define (point? obj)
    (and (vector? obj)
        (eq? (vector-ref obj 0) 'point)
    )
)
(define (point-x obj)
    (if (point? obj) (vector-ref obj 1)
        (display "impossible")
    )
)
(define (point-y obj)
    (if (point? obj) (vector-ref obj 2)
        (display "impossible")
    )
)
(define (point-x-set! obj value)
    (vector-set! obj 1 value)
)
(define (point-y-set! obj value)
    (vector-set! obj 2 value)
)
(define (prime? n)
    (define a (abs n))
    (define (div-a? d)
        (cond 
            ((> (* d d) a) #f) 
            ((= (remainder a d) 0) #t)
            (else (div-a? (+ d 2)))
        )
    )
    (if (not (integer? n)) "impossible"
        (cond
            ((= a 0) #f)
            ((= a 1) #f)
            ((= a 2) #t)
            ((= (remainder a 2) 0) #f)
            (else (not (div-a? 3)))
        )
    )
)
;;; (rank 4 '(1 2 4 8))  =>  3
(define (rank a x)
    (if (not (belongs? a x)) "impossible"
        (+ 1 (length (head a x)))
    )
)
;;; (rational->integer 11/4)  =>  2
(define (rational->integer x)
    (if (not (rational? x)) "impossible"
        (begin
            (define s (if (< x 0) -1 1))
            (define y (abs x))
            (define z (round y))
            (define r (if (> z y) (- z 1) z)) 
            (* s r)
        )
    )
)
;;; (rational->real 11/4)  =>  2.75
(define (rational->real x)
    (if (not (rational? x)) "impossible"
        (+ 0.0 x)
    )
)
(define (remove a x)
    (if (not (belongs? a x)) "impossible"
        (append (head a x) (tail a x))
    )
)
;;; to replace a with b inside x
(define (replace a b x)
    (if (not (belongs? a x)) "impossible"
        (append (head a x) 
            (append (list b) (tail a x))
        )
    )
)
(define (reverse x)
    (define (with a)
        (if (null? a) a
            (cons (last a) (with (truncate a)))
        )
    )
    (if (not (pair? x)) "impossible"
        (with x)
    )
)
(define (sign n) 
    (if (not (real? n)) "impossible"
        (cond 
            ((< n 0) -1) 
            ((= n 0) 0) 
            (else 1)
        )
    )
)
(define (sqrt x) 
    (define (real y) (+ 0.0 y))
    (define (root c)
        (define (with a b) 
            (let ((m (/ (+ a b) 2)))  
                (if (or (= (real a) m) (= m (real b))) m 
                    (if (> (* m m) c) (with a m) (with m b))
                )
            )
        )
        (if (> c 1) (with 1 c) (with c 1))
    )
    (if (not (number? x)) "impossible"
        (cond
            ((= x 0) 0)
            ((and (real? x) (> x 0)) (root x))
            ((and (real? x) (< x 0)) (make-rectangular 0.0 (root (- 0.0 x))))
            ((complex? x) (make-polar (root (magnitude x)) (/ (angle x) 2)))
        )
    )
)
(define (square x)
    (if (not (number? x)) "impossible"
        (* x x)
    )
)
(define (tail a x)
    (if (not (belongs? a x)) (list)
        (if (= a (car x)) (cdr x)
            (tail a (cdr x))
        )
    )
)
;;; example : (translate 'vous)  =>  you
(define (translate pronoun)
    (case pronoun
        ((je)        'I)
        ((tu vous)   'you)
        ((il)        'he)
        ((elle)      'she)
        ((nous)      'we)
        ((ils elles) 'they)
        (else        'unknown)
    )
)
(define (truncate x)
    (if (not pair? x) "impossible"
        (if (null? x) "empty list"
            (if (null? (cdr x)) '()
                (cons (car x) (truncate (cdr x)))
            )
        )
    )
)
(define (vector . args)
    (let ((vec (make-vector (length args))))
        (define (init idx rest-args)
            (if (not (pair? rest-args)) vec
                (begin 
                    (vector-set! vec idx (car rest-args))
                    (init (+ idx 1) (cdr rest-args))
                )
            )
        )
        (init 0 args)
    )
)
(define (open filename)
    (file-open filename 'w)
)
(define (new-line file)
    (begin
        (fdisplay file #\return)
        (fdisplay file #\newline)
    )
)