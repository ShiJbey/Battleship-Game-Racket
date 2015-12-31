;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ship-placement) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
(require 2htdp/image)
(require 2htdp/universe)
(require "battle-ship-GUI.rkt")

(define OCEAN-BG (rectangle 400 400 "solid" "lightblue"))
(define OCEAN-GRID1 OCEAN-GRID)



(define X 15)
(define Y 12)

(define (render-selection abox)
  (place-image/align abox X Y "left" "top"  OCEAN-GRID1))

(define PLACEMENT (render-selection ST-SIZE-5))

(define (select-key-handler animage aSKH)
  (local[(define temp 0)]
  (cond[(key=? aSKH "up") (cond[(= 12 Y)
                                (begin (set! Y (- 412 (image-height animage))) animage)]
                               [else
                                (begin (set! Y (- Y 50)) animage)])]
       [(key=? aSKH "down") (cond[(= 412 (+ Y (image-height animage)))
                                  (begin (set! Y 12) animage)]
                                 [else (begin (set! Y (+ Y 50)) animage)])]
       [(key=? aSKH "right") (cond[(= 415 (+ X (image-width animage)))
                                  (begin (set! X 15) animage)]
                                 [else (begin (set! X (+ X 50)) animage)])]
        [(key=? aSKH "left") (cond[(= 15 X)
                                  (begin (set! X (- 415 (image-width animage))) animage)]
                                 [else (begin (set! X (- X 50)) animage)])]
        [(key=? aSKH "o")
         (rectangle (image-height animage) (image-width animage) "solid" "orangered")]
        [(key=? aSKH "p")
         ST-SIZE-2]
        [(key=? aSKH "s")
         ST-SIZE-3]
        [(key=? aSKH "d")
         ST-SIZE-3]
        [(key=? aSKH "b")
         ST-SIZE-4]
        [(key=? aSKH "a")
         ST-SIZE-5]
        [(key=? aSKH "7")
         (cond[(> (image-height animage) (image-width animage))
               (cond[(image=? animage ST-SIZE-2) (begin (set! OCEAN-GRID1 (place-image/align PATROL-I X Y "left" "top"  OCEAN-GRID1)) animage)]
                    [(image=? animage ST-SIZE-3) (begin (set! OCEAN-GRID1 (place-image/align SUB-I X Y "left" "top"  OCEAN-GRID1)) animage)]
                    [(image=? animage ST-SIZE-4) (begin (set! OCEAN-GRID1 (place-image/align BATTLESHIP-I X Y "left" "top"  OCEAN-GRID1)) animage)]
                    [(image=? animage ST-SIZE-5) (begin (set! OCEAN-GRID1 (place-image/align ACC-I X Y "left" "top"  OCEAN-GRID1)) animage)])]
              [else 
               (cond[(image=? animage (rotate 90 ST-SIZE-2)) (begin (set! OCEAN-GRID1 (place-image/align (rotate 90 PATROL-I) X Y "left" "top"  OCEAN-GRID1)) animage)]
                    [(image=? animage (rotate 90 ST-SIZE-3)) (begin (set! OCEAN-GRID1 (place-image/align (rotate 90 SUB-I) X Y "left" "top"  OCEAN-GRID1)) animage)]
                    [(image=? animage (rotate 90 ST-SIZE-4)) (begin (set! OCEAN-GRID1 (place-image/align (rotate 90 BATTLESHIP-I) X Y "left" "top"  OCEAN-GRID1)) animage)]
                    [(image=? animage (rotate 90 ST-SIZE-5)) (begin (set! OCEAN-GRID1 (place-image/align (rotate 90 ACC-I) X Y "left" "top"  OCEAN-GRID1)) animage)])])] 
       [else animage])))


(define (test-move abox) 
  (big-bang
   abox 
   (on-draw render-selection)
   (on-key select-key-handler)
   ))
          

(test-move ST-SIZE-5)
