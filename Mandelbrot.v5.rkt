#lang racket

;; Required to generate image
(require 2htdp/image)
(require math/flonum)

;; CONSTANTS - NUMBERS

(define INTERATION-DEPTH 120)

(define WIDTH 1800)
(define HEIGHT (* WIDTH 2/3))


;; CONSTANTS - GRAPHIC
(define BACKGROUND 
  (rectangle WIDTH (/ HEIGHT 2) 'solid 'white))

;; Structures
(struct posn [x y] #:transparent)

;; PROCEDURES

;; Posn -> Dot
;; Returns a dot that is colored relative to it's
;; Mandelbrot escape number
(define (pick-dot posn)
  (if (= (third posn) INTERATION-DEPTH)
      (rectangle 1 1 'solid 'black)
      (rectangle 1 1 'solid (make-color 1 1 1 (- 255 (* 2 (third posn)))))))

;; Posn Image -> Image
;; Places a dot on the given image at the location specified 
;; by the posn. Dot color depends on posns third value.
(define (draw posn img)
  (overlay/align/offset
   'left 'top
   (pick-dot posn)
   (- (first posn)) (- (second posn))
   img))

;; Posn -> Number
;; Returns the magnitude of a posn
(define (posn-mag pt)
  (flsqrt (fl+ (fl* (fl (posn-x pt)) (fl (posn-x pt)))
               (fl* (fl (posn-y pt)) (fl (posn-y pt))))))

;; Posn Posn -> Posn
;; Posn addition
(define (posn+ pt1 pt2)
  (posn (fl+ (fl (posn-x pt1)) (fl (posn-x pt2)))
        (fl+ (fl (posn-y pt1)) (fl (posn-y pt2)))))

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

;; Number Number -> Number
;; Returns the mandelbrot escape number for a given complex number
;; given in rectangular coordinates.
(define (mandelbrot-number start)
  (define (iter result count)
    (cond [(> (posn-mag result) 2) count]
          [(> count INTERATION-DEPTH) INTERATION-DEPTH]
          [else (iter (posn+ start (posn-sqr result))
                      (add1 count))]))
  (iter start 0))

;; Number -> Number
;; Returns the scaled location of a point
(define (scaled-x x)
  (- (* 3 (/ x WIDTH)) 2))
(define (scaled-y y)
  (+ (* -2 (/ y HEIGHT)) 1))

;; Creates a nested-list of pairs to represent the location
;; all of the pixels in the half of the scene
(define pixel-location
  (map (λ (x)
         (list (remainder x WIDTH)
               (quotient x WIDTH)
               (mandelbrot-number
                (posn (scaled-x (remainder x WIDTH))
                      (scaled-y (quotient x WIDTH))))))
       (range (/ (* WIDTH HEIGHT) 2))))

;; Generates an image of half the Mandelbrot set usings foldr
(define Mandelbrot-Image
  (foldr (lambda (val bkg)
           (if (< 0 (third val))
               (draw val bkg)
               bkg))
         BACKGROUND
         pixel-location))

;; Complete the image of the Mandelbrot set by flipping the
;; rendered image and placing it underneath the render
(above Mandelbrot-Image
       (flip-vertical Mandelbrot-Image))

;; Time required to generate the list of pixel locations and 
;; related Mandelbrot escape values. Rendering the image takes 
;; around 40 seconds for all examples.
;; image width = 1800
;; depth = 64   --> 29sec
;; depth = 120  --> 46sec
