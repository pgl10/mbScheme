
;;; vector-3d

(define (vec x y z)
  (let ((v (make-vector 3)))
    (vector-set! v 0 x)
    (vector-set! v 1 y)
    (vector-set! v 2 z)
    v))

(define (vx x)
  (vector-ref x 0))

(define (vy x)
  (vector-ref x 1))

(define (vz x)
  (vector-ref x 2))

(define (v+ a b)
  (vec (+ (vx a) (vx b))
       (+ (vy a) (vy b))
       (+ (vz a) (vz b))))

(define (v- a b)
  (vec (- (vx a) (vx b))
       (- (vy a) (vy b))
       (- (vz a) (vz b))))

(define (vscale v x)
  (vec (* x (vx v))
       (* x (vy v))
       (* x (vz v))))

(define (vcross a b)
  (vec
    (- (* (vy a) (vz b)) (* (vy b) (vz a)))
    (- (* (vz a) (vx b)) (* (vz b) (vx a)))
    (- (* (vx a) (vy b)) (* (vx b) (vy a)))))

(define (vdot a b)
  (+ (* (vx a) (vx b))
     (* (vy a) (vy b))
     (* (vz a) (vz b))))

(define (vlen a)
  (sqrt (vdot a a)))

(define (vnormalize a)
  (let ((len (vlen a)))
    (if (> len 1.0e-17)
	(vscale a (/ len))
	a)))

;;; ray

(define (make-ray org dir)
  (cons org dir))

(define (ray-org ray)
  (car ray))

(define (ray-dir ray)
  (cdr ray))

;;; isect

(define (make-isect t p n hit)
  (let ((v (make-vector 4)))
    (vector-set! v 0 t)
    (vector-set! v 1 p)
    (vector-set! v 2 n)
    (vector-set! v 3 hit)
    v))

(define (isect-t i)
  (vector-ref i 0))

(define (isect-p i)
  (vector-ref i 1))

(define (isect-n i)
  (vector-ref i 2))

(define (isect-hit i)
  (vector-ref i 3))

;;; sphere

(define (make-sphere center radius)
  (let ((v (make-vector 2)))
    (vector-set! v 0 center)
    (vector-set! v 1 radius)
    v))

(define (sphere-center s)
  (vector-ref s 0))

(define (sphere-radius s)
  (vector-ref s 1))

;;; plane

(define (make-plane p n)
  (let ((v (make-vector 2)))
    (vector-set! v 0 p)
    (vector-set! v 1 n)
    v))

(define (plane-p p)
  (vector-ref p 0))

(define (plane-n p)
  (vector-ref p 1))

(define (ray-sphere-intersect isect ray sphere)
  (let ((rs (v- (ray-org ray)
    (sphere-center sphere))))
    (let* ((B (vdot rs (ray-dir ray)))
      (C (- (vdot rs rs) (square (sphere-radius sphere))))
      (D (- (* B B) C)))
      (if (> D 0)
        (let ((t (- 0.0 B (sqrt D))))
          (if (< 0.0 t (isect-t isect))
            (let* ((hit 1)
              (p (v+ (ray-org ray) (vscale (ray-dir ray) t)))
              (n (v- p (sphere-center sphere))))
              (set! isect (make-isect t p (vnormalize n) hit))
            )
          )
        )
      )
      isect
    )
  )
)

(define (ray-plane-intersect isect ray plane)
  (let ((d (- (vdot (plane-p plane) (plane-n plane))))
        (v (vdot (ray-dir ray) (plane-n plane)))
       )
    (if (>= (abs v) 1.0e-17)
      (let ((t (- (/ (+ (vdot (ray-org ray) (plane-n plane)) d) v))))
        (if (< 0.0 t (isect-t isect))
          (let ((hit 1)
            (p (v+ (ray-org ray) (vscale (ray-dir ray) t)))
            (n (plane-n plane)))
            (set! isect (make-isect t p n hit))
          )
        )
      )
    )
    isect
  )
)
