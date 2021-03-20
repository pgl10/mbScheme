;;; make-matrix creates a matrix (a vector of vectors).
(define (make-matrix rows cols)
    (define (line m i)
        (if (< i rows) (begin (vector-set! m i (make-vector cols)) (line m (+ i 1))) m)
    )
    (define m (make-vector rows))
    (line m 0)
)
;;; matrix-print displays a matrix.
(define (matrix-print m)
    (define rows (matrix-rows m))
    (define cols (matrix-cols m))
    (define (row i)
        (if (< i rows) (begin (display (matrix-get-row m i)) (newline) (row (+ i 1))))
    )
    (row 0)
)
;;; matrix-ref returns the kth element of the ith row of a matrix.
(define (matrix-ref m i k)
    (vector-ref (vector-ref m i) k)
)
;;; matrix-set! changes the kth element of the ith row of a matrix.
(define (matrix-set! m i k x)
    (vector-set! (vector-ref m i) k x)
)
;;; matrix-rows returns the number of rows in a matrix.
(define (matrix-rows m)
    (vector-length m)
)
;;; matrix-cols returns the number of columns in a matrix.
(define (matrix-cols m)
    (vector-length (vector-ref m 0))
)
;;; matrix-copy creates the copy of a matrix.
(define (matrix-copy m)
    (define rows (matrix-rows m))
    (define cols (matrix-cols m))
    (define (line i k)
        (if (< k cols) (begin (matrix-set! r i k (matrix-ref m i k)) (line i (+ k 1))))
    )
    (define (copy i)
        (if (< i rows) (begin (line i 0) (copy (+ i 1))))
    )
    (define r (make-matrix rows cols))
    (copy 0)
    r
)
;;; matrix-transpose creates the transpose of a matrix.
(define (matrix-transpose m)
    (define rows (matrix-rows m))
    (define cols (matrix-cols m))
    (define (line i k)
        (if (< k cols) (begin (matrix-set! r k i (matrix-ref m i k)) (line i (+ k 1))))
    )
    (define (copy i)
        (if (< i rows) (begin (line i 0) (copy (+ i 1))))
    )
    (define r (make-matrix cols rows))
    (copy 0)
    r
)
;;; matrix-get-row gets a row of a matrix.
(define (matrix-get-row m i)
    (define cols (matrix-cols m))
    (define (copy k)
        (if (< k cols) (begin (vector-set! r k (matrix-ref m i k)) (copy (+ k 1))))
    )
    (define r (make-vector cols))
    (copy 0)
    r
)
;;; matrix-set-row sets a vector on a row of a matrix.
(define (matrix-set-row m i v)
    (define cols (matrix-cols m))
    (define vlen (vector-length v))
    (define (copy k)
        (if (< k vlen) (begin (vector-set! (vector-ref m i) k (vector-ref v k)) (copy (+ k 1))))
    )
    (if (not (= vlen cols)) "impossible"
        (copy 0)
    )
)
;;; matrix-get-col gets a column of a matrix.
(define (matrix-get-col m k)
    (define rows (matrix-rows m))
    (define (copy i)
        (if (< i rows) (begin (vector-set! r i (matrix-ref m i k)) (copy (+ i 1))))
    )
    (define r (make-vector rows))
    (copy 0)
    r
)
;;; matrix-set-col sets a vector on a column of a matrix.
(define (matrix-set-col m k v)
    (define rows (matrix-rows m))
    (define vlen (vector-length v))
    (define (copy i)
        (if (< i vlen) (begin (vector-set! (vector-ref m i) k (vector-ref v i)) (copy (+ i 1))))
    )
    (if (not (= vlen rows)) "impossible"
        (copy 0)
    )
)
;;; matrix-mul-sca multiplies on itself a matrix by a scalar.
(define (matrix-mul-sca m s)
    (define rows (matrix-rows m))
    (define cols (matrix-cols m))
    (define (line i k)
        (if (< k cols) (begin (vector-set! (vector-ref m i) k (* (matrix-ref m i k) s)) (line i (+ k 1))))
    )
    (define (mul i)
        (if (< i rows) (begin (line i 0) (mul (+ i 1))))
    )
    (mul 0)
)
;;; matrix-mul-vec creates a vector by multiplication of a matrix by a vector.
(define (matrix-mul-vec m v)
    (define rows (matrix-rows m))
    (define cols (matrix-cols m))
    (define vlen (vector-length v))
    (define r (make-vector rows))
    (define sum 0)
    (define (add i k)
        (if (< k cols) (begin (set! sum (+ (* (vector-ref (vector-ref m i) k) (vector-ref v k)) sum)) (add i (+ k 1))) sum)
    )
    (define (mul i)
        (if (< i rows) (begin (set! sum 0) (vector-set! r i (add i 0)) (mul (+ i 1))) r)
    )
    (if (not (= vlen cols)) "impossible"
        (mul 0)
        r
    )
)
;;; matrix-mul-mat creates a matrix by multiplication of a matrix by a matrix.
(define (matrix-mul-mat a b)
    (define nra (matrix-rows a))
    (define nca (matrix-cols a))
    (define nrb (matrix-rows b))
    (define ncb (matrix-cols b))
    (define r (make-matrix nra ncb))
    (define sum 0)
    (define (mul i k n)
        (if (< n nca) (begin (set! sum (+ (* (matrix-ref a i n) (matrix-ref b n k)) sum)) (mul i k (+ n 1))) sum)
    )
    (define (colb i k)
        (if (< k ncb) (begin (set! sum 0) (mul i k 0) (matrix-set! r i k sum) (colb i (+ k 1))))
    )
    (define (rowa i)
        (if (< i nra) (begin (colb i 0) (rowa (+ i 1))) r)
    )
    (if (not (= nca nrb)) "impossible"
        (rowa 0)
        r
    )
)
;;; matrix-unit creates a square identity matrix.
(define (matrix-unit n)
    (define r (make-matrix n n))
    (define (col i k)
        (if (< k n) (begin (matrix-set! r i k (if (= i k) 1 0)) (col i (+ k 1))))
    )
    (define (row i)
        (if (< i n) (begin (col i 0) (row (+ i 1))) r)
    )
    (row 0)
    r
)
;;; matrix-minor creates a minor of a matrix.
(define (matrix-minor m i k)
    (define rows (- (matrix-rows m) 1))
    (define cols (- (matrix-cols m) 1))
    (define r (make-matrix rows cols))
    (define i1 0)
    (define k1 0)
    (define (line i0 k0)
        (if (< k0 cols) (begin (if (= k0 k) (set! k1 1)) (matrix-set! r i0 k0 (matrix-ref m (+ i0 i1) (+ k0 k1))) (line i0 (+ k0 1))))
    )
    (define (copy i0)
        (if (< i0 rows) (begin (if (= i0 i) (set! i1 1)) (set! k1 0) (line i0 0) (copy (+ i0 1))))
    )
    (copy 0)
    r
)
;;; matrix-det computes the determinant of a matrix.
(define (matrix-det m)
    (define n (matrix-rows m))
    (define (sarrus m)
        (define a11 (matrix-ref m 0 0))
        (define a12 (matrix-ref m 0 1))
        (define a13 (matrix-ref m 0 2))
        (define a21 (matrix-ref m 1 0))
        (define a22 (matrix-ref m 1 1))
        (define a23 (matrix-ref m 1 2))
        (define a31 (matrix-ref m 2 0))
        (define a32 (matrix-ref m 2 1))
        (define a33 (matrix-ref m 2 2))
        (define s1 (+ (* a11 a22 a33) (* a12 a23 a31) (* a13 a21 a32)))
        (define s2 (+ (* a13 a22 a31) (* a11 a23 a32) (* a12 a21 a33)))
        (- s1 s2)
    )
    (define r 1)
    (define (max k i)
        (define x i)
        (define (maxi k i)
            (if (< k n) (begin (if (> (abs (matrix-ref s k i)) (abs (matrix-ref s x i))) (set! x k)) (maxi (+ k 1) i)) x)
        )
        (maxi i i)
    )
    (define (swap i im)
        (define x 0)
        (define (exch i im k)
            (if (< k n) 
                (begin 
                    (set! x (matrix-ref s i k)) 
                    (matrix-set! s i k (matrix-ref s im k)) 
                    (matrix-set! s im k x) 
                    (exch i im (+ k 1))
                )
            )
        )
        (exch i im i)
        (set! r (- r))
    )
    (define (div i)
        (define p (matrix-ref s i i))
        (define (divi i k)
            (if (< k n) (begin (matrix-set! s i k (/ (matrix-ref s i k) p)) (divi i (+ k 1))) 
            )
        )
        (if (= p 0) (set! r 0) (divi i i))
    )
    (define (nul k i)
        (define (sub k i t)
            (if (< t n) (begin (matrix-set! s k t (- (matrix-ref s k t) (* (matrix-ref s i t) x))) (sub k i (+ t 1)))
            )
        )
        (if (< k n) (begin (define x (matrix-ref s k i)) (sub k i i) (nul (+ k 1) i))
        )
    )
    (define (gauss i)
        (if (< i n) 
            (begin 
                (define im (max i i)) 
                (if (not (= im i)) (swap i im))
                (set! r (* r (matrix-ref s i i)))
                (div i)
                (nul (+ i 1) i)
                (gauss (+ i 1))
            ) 
            r
        )
    )
    (define s (matrix-copy m))
    (cond
        ((not (= (vector-length (vector-ref m 0)) n)) "impossible")
        ((= n 1) (matrix-ref m 0 0))
        ((= n 2) (- (* (matrix-ref m 0 0) (matrix-ref m 1 1)) (* (matrix-ref m 0 1) (matrix-ref m 1 0))))
        ((= n 3) (sarrus m))
        (else (gauss 0))
    )
)
;;; matrix-sys-eqt creates the solution of a system of linear equations.
(define (matrix-sys-eqt a b)
    (define rows (matrix-rows a))
    (define cols (matrix-cols a))
    (define blen (vector-length b))
    (define r (make-vector rows))
    (define d (matrix-det a))
    (define (solve i)
        (if (< i blen) (begin (define m (matrix-copy a)) (matrix-set-col m i b) (vector-set! r i (/ (matrix-det m) d)) (solve (+ i 1))) r)
    )
    (if (or (not (= blen cols)) (= d 0)) "impossible"
        (solve 0)
    )
)
;;; matrix-inv creates the inverse of a matrix.
(define (matrix-inv m)
    (define rows (matrix-rows m))
    (define cols (matrix-cols m))
    (define r (make-matrix rows cols))
    (define d (matrix-det m))
    (define (u i) 
        (define r (make-vector cols))
        (define (null k)
            (if (< k cols) (begin (vector-set! r k 0) (null (+ k 1))))
        )
        (null 0)
        (vector-set! r i 1)
        r
    )
    (define (eqt i)
        (if (< i cols) (begin (matrix-set-col r i (matrix-sys-eqt m (u i))) (eqt(+ i 1))) r)
    )
    (if (or (not (= rows cols)) (= d 0)) "impossible"
        (eqt 0)
    )
)
