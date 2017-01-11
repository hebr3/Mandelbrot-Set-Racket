#lang typed/racket
(require math/flonum
         images/flomap
         threading)

;; CONSTANTS
(define DEPTH 256)

(define CENTER (make-flrectangular 0.5 0.0))
(define X-MIN -2)
(define X-MAX 1)
(define Y-MAX 3/2)
(define Y-MIN (- Y-MAX (* 1 (- X-MAX X-MIN))))

(define WIDTH 2000)
(define HEIGHT (floor (* WIDTH (/ (- Y-MAX Y-MIN)
                                  (- X-MAX X-MIN)))))

;; PROCEDURES
;(: magnitude>2? (-> Float-Complex Boolean))
;(define (magnitude>2? num)
;  (let ([x (real-part num)]
;        [y (imag-part num)])
;    (fl> (fl+ (fl* x x) (fl* y y))
;         4.0)))

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

(: mandelbrot-number (-> Float-Complex Natural Natural))
(define (mandelbrot-number num d)
  (: iter (-> Float-Complex Natural Natural))
  (define (iter comp count)
    (let ([x (real-part comp)]
          [y (imag-part comp)])
      (cond [(> (+ (sqr x) (sqr y)) 4.0) count]
            [(> count d) d]
            [else (iter (+ num (* comp comp))
                        (add1 count))])))
  (iter num 0))

(: mandelbrot (-> Integer Integer Integer Integer Integer))
(define (mandelbrot iterations x y n)
  (let ([ci (fl- (fl/ (* 2.0 (->fl y)) (->fl n)) 1.0)]
        [cr (fl- (fl/ (* 2.0 (->fl x)) (->fl n)) 1.5)])
    (let loop ([i 0] [zr 0.0] [zi 0.0])
      (if (> i iterations)
          i
          (let ([zrq (fl* zr zr)]
                [ziq (fl* zi zi)])
            (cond
              [(fl> (fl+ zrq ziq) 4.0) i]
              [else (loop (add1 i)
                          (fl+ (fl- zrq ziq) cr)
                          (fl+ (fl* 2.0 (fl* zr zi)) ci))]))))))


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
                                                   (scale-y y)) DEPTH)])
    (vector (if (= mn DEPTH) 1.0 (fl (- 1 (/ mn DEPTH))))
            0.0 0.0
            (if (= mn DEPTH) 0.0 (fl (/ mn DEPTH))))))

(: mand-helper (-> Natural Natural
                         (Vector Real Real Real Real)))
(define (mand-helper x y)
  (let ([mn (mandelbrot DEPTH x y 1500)])
    (vector (if (>= mn DEPTH) 1.0 (fl (- 1 (/ mn DEPTH))))
            0.0 0.0
            (if (>= mn DEPTH) 0.0 (fl (/ mn DEPTH))))))

(time
(flomap->bitmap
 (flomap-scale
  (build-flomap*
   4 WIDTH HEIGHT
   (λ (x y)
    (mandelbrot-vector x y))) 1/4)))


(~> (λ (x y) (mand-helper x y))
    (build-flomap* 4 WIDTH HEIGHT _)
    (flomap-scale _ 1/4)
    flomap->bitmap
    time)


;(time (mandelbrot-number -0.7+0.0i 100000000))
;(time (mandelbrot 100000000 400 500 1000))