#lang racket

;; Required to generate image
(require 2htdp/image)

;; CONSTANTS - NUMBERS

(define INTERATION-DEPTH 8)

(define WIDTH 300)
(define HEIGHT (* WIDTH 2/3))


;; CONSTANTS - GRAPHIC
(define BACKGROUND 
  (rectangle WIDTH (/ HEIGHT 2) 'solid 'white))

(define dot1 (rectangle 1 1 'solid 'Gainsboro))
(define dot2 (rectangle 1 1 'solid 'Silver))
(define dot3 (rectangle 1 1 'solid 'DarkGray))
(define dot4 (rectangle 1 1 'solid 'DimGray))
(define dot5 (rectangle 1 1 'solid 'Black))

;; PROCEDURES

;; Posn -> Dot
;; Returns a dot that is colored relative to it's
;; Mandelbrot escape number
(define (pick-dot posn)
  (cond [(<= INTERATION-DEPTH (third posn)) dot5]
        [(= 1 (third posn)) dot1]
        [(<= 2 (third posn) 3) dot2]
        [(<= 4 (third posn) 6) dot3]
        [(<= 7 (third posn) INTERATION-DEPTH) dot4]))

;; Posn Image -> Image
;; Places a dot on the given image at the location specified 
;; by the posn. Dot color depends on posns third value.
(define (draw posn img)
  (overlay/align/offset
   'left 'top
   (pick-dot posn)
   (- (first posn)) (- (second posn))
   img))

;; Complex -> Number
;; Returns the mandelbrot escape number for a given complex number.
(define (mandelbrot-number start)
  (define (iter result last count)
    (cond [(> (magnitude result) 2) count]
          [(> count INTERATION-DEPTH) INTERATION-DEPTH]
          [else (iter (+ start (sqr result))
                      result
                      (add1 count))]))
  (cond [(> (magnitude start) 2.0) 0]
        [(< (magnitude (+ start 1.0+0.0i)) 0.24) INTERATION-DEPTH]
        [(< (magnitude (+ start 0.25+0.0i)) 0.5) INTERATION-DEPTH]
        [(< (magnitude (+ start 1.315+0.0i)) 0.05) INTERATION-DEPTH]
        [(< (magnitude (+ start -0.28-0.531i)) 0.04) INTERATION-DEPTH]
        [(< (magnitude (+ start 0.13-0.73i)) 0.09) INTERATION-DEPTH]
        [(< (magnitude (+ start 0.02-0.309i)) 0.3) INTERATION-DEPTH]
        [(< (magnitude (+ start 0.244-0.13i)) .45) INTERATION-DEPTH]
        [(< (magnitude (+ start 0.134-0.198i)) 0.43) INTERATION-DEPTH]
        [(< (magnitude (+ start -0.059-0.236i)) 0.3) INTERATION-DEPTH]
        [(< (magnitude (+ start 0.51-0.55i)) 0.033) INTERATION-DEPTH]
        [else (iter start start 0)]))

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
                (make-rectangular (scaled-x (remainder x WIDTH))
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
;; around 3 seconds for all examples.
;; c = 6  --> ~3sec
;; c = 7  --> ~6sec
;; c = 8  --> ~11sec
;; c = 9  --> ~20sec
;; c = 10 --> ~41sec
;; c = 11 --> ~97sec
;; c = 12 --> ~241sec
;; c = 13 --> ~747sec  > 12min
;; c = 14 --> ~2378sec > 39min
