
(define (zero? x)
  (if (= x 0) #t #f)
)

(define (exact x)
  (if (< x 0)
    (if (< (round x) x) (+ (round x) 1) (round x))
    (if (> (round x) x) (- (round x) 1) (round x))
  )
)

(define (orthobasis n)
  (let ((v (cond
         ((< -0.6 (vx n) 0.6) (vec 1.0 0.0 0.0))
         ((< -0.6 (vy n) 0.6) (vec 0.0 1.0 0.0))
         ((< -0.6 (vz n) 0.6) (vec 0.0 0.0 1.0))
         (else (vec 1.0 0.0 0.0)))
        )
       )
    (let* ((s (vnormalize (vcross v n)))
           (t (vnormalize (vcross n s))))
           (vector s t n))
  )
)

(define (clamp f)
  (let ((i (exact (floor (* f 255.5)))))
    (cond
      ((< i 0) 0)
      ((> i 255) 255)
      (else i)
    )
  )
)

