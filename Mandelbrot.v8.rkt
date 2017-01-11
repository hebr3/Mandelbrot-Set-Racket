#lang racket

;; Required to generate image
(require picturing-programs)
(require math/flonum)


;; CONSTANTS - NUMBERS
(define DEPTH 64)
(define WIDTH 1800)
(define HEIGHT (* WIDTH 2/3))


;; CONSTANTS - GRAPHIC
(define BACKGROUND 
  (rectangle WIDTH HEIGHT 'solid 'grey))


;; Structures
(struct posn [x y] #:transparent)


;; PROCEDURES

;; Posn -> Number
;; Returns the magnitude of the posn
(define (posn-mag pt)
  (let ([pt-x (fl (posn-x pt))]
        [pt-y (fl (posn-y pt))])
    (flsqrt (fl+ (fl* pt-x pt-x)
                 (fl* pt-y pt-y)))))

;; Posn Posn -> Posn
;; Posn addition
(define (posn+ pt1 pt2)
  (let ([pt1-x (fl (posn-x pt1))]
        [pt1-y (fl (posn-y pt1))]
        [pt2-x (fl (posn-x pt2))]
        [pt2-y (fl (posn-y pt2))])
    (posn (fl+ pt1-x pt2-x)
          (fl+ pt1-y pt2-y))))

;; Posn Posn -> Posn
;; Posn multiplication
(define (posn* pt1 pt2)
  (let ([x1 (fl (posn-x pt1))]
        [y1 (fl (posn-y pt1))]
        [x2 (fl (posn-x pt2))]
        [y2 (fl (posn-y pt2))])
    (posn (fl- (fl* x1 x2) (fl* y1 y2))
          (fl+ (fl* x1 y2) (fl* x2 y1)))))

;; Posn -> Posn
;; Posn square
(define (posn-sqr pt)
  (posn* pt pt))

;; Posn -> Number
;; Returns the mandelbrot escape number for a given complex number
;; given in rectangular coordinates.
(define (mandelbrot-number start)
  (define (iter result count)
    (cond [(> (posn-mag result) 2) (sub1 count)]
          [(> count DEPTH) DEPTH]
          [else (iter (posn+ start (posn-sqr result))
                      (add1 count))]))
  (iter start 1))

;; Number -> Number
;; Returns the scaled location of a point
(define (scaled-x x)
  (- (* 3 (/ x WIDTH)) 2))
(define (scaled-y y)
  (+ (* -2 (/ y HEIGHT)) 1))


;;;;;;;;;;;
(define c (current-inexact-milliseconds))
;;;;;;;;;;;


;; Generates image
(define M-Image
  (map-image
   (λ (x y c)
     (let* ([ref (mandelbrot-number (posn (scaled-x x) (scaled-y y)))]
            [color (λ (x)
                     (inexact->exact (floor (* 160 (acos (/ ref DEPTH))))))])
       (cond [(= ref DEPTH) (name->color 'black)]
             [else (make-color 1 1 (+ 50 ref)
                               (color ref))])))
 BACKGROUND))

M-Image

;;;;;;;;;;;
(define d (current-inexact-milliseconds))
(/ (- d c) 1000)
;;;;;;;;;;;

;; Time required to generate the list of pixel locations and 
;; related Mandelbrot escape values.
;; image width = 1800
;; depth = 32  --> 39sec
;; depth = 64  --> 45sec
;; depth = 120 --> 83sec
;; depth = 180 --> 93sec