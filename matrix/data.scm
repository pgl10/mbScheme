(define v0 (vector 11 12 13))
(define v1 (vector 21 22 23))
(define v2 (vector 31 32 33))
(define vv (vector v0 v1 v2))
(define w (vector v0 v1))
(define wt (matrix-transpose w))
(define mm (make-matrix 3 3))
(define r0 (vector 4 8 7 9))
(define r1 (vector 6 8 2 4))
(define r2 (vector 5 7 6 3))
(define r3 (vector 1 3 7 9))
(define M4 (vector r0 r1 r2 r3))
(define n0 (vector 5 7 8 9 2 4))
(define n1 (vector 7 4 5 8 5 2))
(define n2 (vector 5 6 5 4 7 5))
(define n3 (vector 8 5 4 7 6 2))
(define n4 (vector 2 3 2 1 4 5))
(define n5 (vector 8 5 6 3 2 1))
(define M6 (vector n0 n1 n2 n3 n4 n5))
(define m0 (vector 5 7 8 9 2 4 8 7))
(define m1 (vector 7 4 5 8 5 2 3 1))
(define m2 (vector 5 6 5 4 7 5 2 3))
(define m3 (vector 8 5 4 7 6 2 1 5))
(define m4 (vector 2 3 2 1 4 5 7 8))
(define m5 (vector 8 5 6 3 2 1 4 7))
(define m6 (vector 1 5 9 7 5 3 6 5))
(define m7 (vector 7 5 6 4 8 5 2 1))
(define M8 (vector m0 m1 m2 m3 m4 m5 m6 m7))
(define a0 (vector 5 7 8 9 2 4 8 7 9 4))
(define a1 (vector 7 4 5 8 5 2 3 1 4 2))
(define a2 (vector 5 6 5 4 7 5 2 3 1 6))
(define a3 (vector 8 5 4 7 6 2 1 5 6 3))
(define a4 (vector 2 3 2 1 4 5 7 8 5 1))
(define a5 (vector 8 5 6 3 2 1 4 7 8 7))
(define a6 (vector 1 5 9 7 5 3 6 5 4 6))
(define a7 (vector 7 5 6 4 8 5 2 1 3 2))
(define a8 (vector 8 5 2 3 6 5 4 7 9 7))
(define a9 (vector 2 6 4 8 5 3 7 9 1 5))
(define MA (vector a0 a1 a2 a3 a4 a5 a6 a7 a8 a9))
(define f0 (vector 5 7 8 9 2 4 8 7 9 4 6 8 4 2 1 4))
(define f1 (vector 7 4 5 8 5 2 3 1 4 2 2 6 4 7 8 2))
(define f2 (vector 5 6 5 4 7 5 2 3 1 6 4 8 2 3 1 5))
(define f3 (vector 8 5 4 7 6 2 1 5 6 3 6 9 3 2 4 7))
(define f4 (vector 2 3 2 1 4 5 7 8 5 1 3 4 6 8 7 6))
(define f5 (vector 8 5 6 3 2 1 4 7 8 7 2 5 8 7 4 3))
(define f6 (vector 1 5 9 7 5 3 6 5 4 6 1 4 7 9 6 2))
(define f7 (vector 7 5 6 4 8 5 2 1 3 2 2 4 8 6 3 7))
(define f8 (vector 8 5 2 3 6 5 4 7 9 7 6 5 4 1 2 9))
(define f9 (vector 2 6 4 8 5 3 7 9 1 5 6 5 2 3 4 4))
(define fa (vector 8 5 6 5 4 1 2 4 7 6 2 5 4 6 1 6))
(define fb (vector 9 5 1 2 3 4 5 6 8 7 4 2 5 1 6 3))
(define fc (vector 4 2 3 5 6 8 4 1 2 5 4 7 4 2 9 2))
(define fd (vector 7 5 3 6 5 1 2 3 6 4 7 8 5 4 2 8))
(define fe (vector 3 2 6 5 4 8 7 5 4 2 6 9 8 7 5 3))
(define ff (vector 1 4 5 8 4 5 6 3 2 5 8 7 4 8 9 8))
(define MG (vector f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 fa fb fc fd fe ff))
