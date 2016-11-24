#lang racket

;; Required to generate image
(require 2htdp/image)

;; CONSTANTS - NUMBERS
(define INTERATION-DEPTH 14)

(define WIDTH 300)
(define HEIGHT (* WIDTH 2/3))


;; CONSTANTS - GRAPHIC
(define Background 
  (rectangle WIDTH (/ HEIGHT 2) 'solid 'white))

(define dot1 (rectangle 1 1 'solid 'Gainsboro))
(define dot2 (rectangle 1 1 'solid 'Silver))
(define dot3 (rectangle 1 1 'solid 'DarkGray))
(define dot4 (rectangle 1 1 'solid 'DimGray))
(define dot5 (rectangle 1 1 'solid 'Black))

;; PROCEDURES

;; Posn -> Dot
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
  (iter start start 0))

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
  (iter start start 0))

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
;; depth = 6  --> ~5sec
;; depth = 7  --> ~11sec
;; depth = 8  --> ~23sec
;; depth = 9  --> ~50sec
;; depth = 10 --> ~111sec
