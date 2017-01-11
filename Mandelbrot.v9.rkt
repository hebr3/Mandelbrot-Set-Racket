#lang typed/racket
(require math/flonum
         images/flomap)

;; CONSTANTS
(define DEPTH 64)

(define CENTER (make-flrectangular 0.5 0.0))
(define X-MIN -2)
(define X-MAX 1/2)
(define Y-MAX 5/4)
(define Y-MIN (- Y-MAX (* 2/3 (- X-MAX X-MIN))))

(define WIDTH 2000)
(define HEIGHT (floor (* WIDTH (/ (- Y-MAX Y-MIN)
                                  (- X-MAX X-MIN)))))

;; PROCEDURES
(: magnitude>2? (-> Float-Complex Boolean))
(define (magnitude>2? num)
  (let ([x (real-part num)]
        [y (imag-part num)])
    (fl> (flsqrt (fl+ (fl* x x) (fl* y y)))
         2.0)))

(: complex-sqr (-> Float-Complex Float-Complex))
(define (complex-sqr num)
  (let* ([x (real-part num)]
         [y (imag-part num)]
         [r (fl- (fl* x x) (fl* y y))]
         [c (fl* 2.0 (fl* x y))])
    (make-flrectangular r c)))

(: complex-plus (-> Float-Complex Float-Complex Float-Complex))
(define (complex-plus n1 n2)
  (let ([x1 (real-part n1)]
        [y1 (imag-part n1)]
        [x2 (real-part n2)]
        [y2 (imag-part n2)])
  (make-flrectangular (fl+ x1 x2) (fl+ y1 y2))))

(: mandelbrot-number (-> Float-Complex Natural))
(define (mandelbrot-number num)
  (: iter (-> Float-Complex Natural Natural))
  (define (iter comp count)
    (cond [(magnitude>2? comp) count]
          [(> count DEPTH) DEPTH]
          [else (iter (complex-plus num (complex-sqr comp))
                      (add1 count))]))
  (iter num 0))

(: scale-x (-> Natural Float))
(define (scale-x num)
  (let ([w (- X-MAX X-MIN)])
    (fl (+ X-MIN (* num (/ w WIDTH))))))
(: scale-y (-> Natural Float))
(define (scale-y num)
  (let ([h (- Y-MAX Y-MIN)])
    (fl (- Y-MAX (* num (/ h HEIGHT))))))

(: mandelbrot-vector (-> Natural Natural
                         (Vector Real Real Real Real)))
(define (mandelbrot-vector x y)
  (let ([mn (mandelbrot-number (make-flrectangular (scale-x x)
                                                   (scale-y y)))])
    (vector (if (= mn DEPTH) 1.0 (fl (- 1 (/ mn DEPTH))))
            0.0 0.0
            (if (= mn DEPTH) 0.0 (fl (/ mn DEPTH))))))

(time
(flomap->bitmap
 (flomap-scale
  (build-flomap*
   4 WIDTH HEIGHT
   (λ (x y)
    (mandelbrot-vector x y))) 1/2))
)
