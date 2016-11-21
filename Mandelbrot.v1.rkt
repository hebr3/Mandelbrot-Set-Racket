#lang racket

;; Required to generate image
(require 2htdp/image)

;; CONSTANTS - NUMBERS
(define WIDTH 300)
(define HALF-W (* WIDTH 2/3))
(define HEIGHT (* WIDTH 2/3))
(define HALF-H (* HEIGHT 1/2))


;; CONSTANTS - GRAPHIC
(define Background
  (scene+line
   (scene+line (empty-scene WIDTH HEIGHT)
               0 HALF-H WIDTH HALF-H
               'black)
   HALF-W 0 HALF-W HEIGHT
   'black))

(define dot (rectangle 1 1 'solid 'black))


;; PROCEDURES

;; Posn -> Posn
;; Squares a posn using the rules for squaring a complex number
(define (sqr-posn pt)
  (let [(x (first pt))
        (y (second pt))]
    (list (- (sqr x) (sqr y))
          (+ (* 2 x y)))))

;; Posn Posn -> Posn
;; Addes two posn using the rules for adding complex numbers
(define (posn+ pt1 pt2)
  (list (+ (first pt1) (first pt2))
        (+ (second pt1) (second pt2))))

;; Posn -> Distance
;; Calculate the magnitude of a posn using rules for complex numbers 
(define (mag-posn pt)
  (sqrt (+ (sqr (first pt)) (sqr (second pt)))))

;; Posn -> Boolean
;; Predicate that determines whether a given Posn (complex)
;; number is in the Mandelbrot set.
(define (mandelbrot? c)
  (define (iter z count)
    (cond [(> (mag-posn z) 2) #f]
          [(> count 7) #t]
          [else (iter (posn+ c (sqr-posn z)) (add1 count))]))
  (iter c 0)) 

;; Creates a range of numbers to represent all pixels
;; in the scene
(define pixel-count (range (* WIDTH HEIGHT)))

;; Number -> (list x y)
;; Converts a given number into a position pair in the scene
(define (number->posn n)
  (list (remainder n WIDTH)
        (quotient n WIDTH)))

;; Creates a range of position pairs that represent all
;; of the pixels in the scene.
(define pixel-location
  (map number->posn pixel-count))

;; Posn Image -> Image
;; Places a dot on the given image at the location given
;; in the posn. Position given from bottom right corner.
(define (draw posn img)
  (overlay/align/offset
   'right 'bottom
   dot
   (first posn) (second posn)
   img))

;; Posn -> Complex
;; Converts a given Posn into a Complex number
;; '(1 2) -> 1+2i
(define (rectangular->complex x)
  (make-rectangular (first x) (second x)))

;; Posn -> Posn
;; Converts a Pixel Location to a relative Pixel Location
;; based on the image representing the graph of
;; -2 < x < 1 and -1 < y < 1 
;; '(0 0) -> '(1 1)
;; '(299 199) -> '(-2 1)
(define (convert pt)
  (list (+ (* -3 (/ (first pt) WIDTH)) 1)
        (- (* 2 (/ (second pt) HEIGHT)) 1)))

;; Generate an image of the Mandelbrot set usings foldr
;; and the mandelbrot? predicate
(foldr (lambda (val bkg) 
         (if (mandelbrot? (convert val))
             (draw val bkg)
             bkg))
       Background
       pixel-location)

;; Time required to generate the image based on count limit
;; in mandelbrot? function.
;; count > 6  --> ~20sec
;; count > 7  --> ~31sec
;; count > 8  --> ~55sec
;; count > 9  --> ~119sec
;; count > 10 --> ~264sec
;; count > 11 --> ~702sec
