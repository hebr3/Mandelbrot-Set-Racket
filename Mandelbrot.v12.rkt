#lang typed/racket/gui
(require math/flonum)

(define SCALE 0.00000000000001)
(define CENTER '(-0.630002497535 0.4000025025))
(define WIDTH 500)
(define DEPTH 600)
(define x0 (fl- (fl* 0.5 (* SCALE WIDTH)) (first CENTER)))
(define y0 (fl+ (fl* 0.5 (* SCALE WIDTH)) (second CENTER)))

(: mandelbrot (-> Float Float Integer))
(define (mandelbrot x y)
  (let loop : Integer
    ([i 0] [zr 0.0] [zi 0.0])
    (if (> i DEPTH)
        i
        (let ([zrq (fl* zr zr)]
              [ziq (fl* zi zi)])
          (cond
            [(fl> (fl+ zrq ziq) 4.0) i]
            [else (loop (add1 i)
                        (fl+ (fl- zrq ziq) x)
                        (fl+ (fl* 2.0 (fl* zr zi)) y))])))))

(define black (make-object color% "black"))

(: f (-> Float Byte))
(define (f x)
  (let ([fx (exact-floor (fl* 255.0 (expt (cos x) 2)))])
    (if (byte? fx) fx 0)))

(: fr (-> Integer Byte))
(define (fr x) (f (fl/ (->fl x) 80.0)))
(: fg (-> Integer Byte))
(define (fg x) (f (fl+ (fl/ (->fl x) 80.0) (* pi 2/3))))
(: fb (-> Integer Byte))
(define (fb x) (f (fl+ (fl/ (->fl x) 80.0) (* pi 4/3))))

(: rgb (-> Integer (Instance Color%)))
(define (rgb x)
  (if (> x DEPTH)
      black
      (make-color (fr x) (fg x) (fb x) 1)))

(define target (make-bitmap WIDTH WIDTH))
(define dc (new bitmap-dc% [bitmap target]))

(send dc set-smoothing 'aligned)

(define t3
  (time
   (for*/vector : (Vectorof (Vector Integer Integer Integer))
     ([x (range WIDTH)][y (range WIDTH)])
     (vector x y (mandelbrot (fl- (fl* SCALE (->fl x)) x0)
                           (fl- y0 (fl* SCALE (->fl y))))))))
(time
 (for ([pt t3])
   (send dc set-pixel (vector-ref pt 0) (vector-ref pt 1)
         (rgb (vector-ref pt 2)))))

target
;(send target save-file "test.png" 'png)

(define depth-list
  (for/list : (Listof Integer)
    ([p t3]) (vector-ref p 2)))

(define avg-depth
  (/ (apply + depth-list)
     (length depth-list)))
(define max-depth
  (apply max depth-list))
(define min-depth
  (apply min depth-list))

(define depth-hash
  (for/list : (Listof (List Integer Integer))
    ([x DEPTH])
    (list x (count (λ ([y : Integer]) (= x y)) depth-list))))

(define depth-hash2
  (filter (λ ([x : (List Integer Integer)]) (not (= 0 (second x))))
          depth-hash))

(require plot)
(plot (discrete-histogram depth-hash2))