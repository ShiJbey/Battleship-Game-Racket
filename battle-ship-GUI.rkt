;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname battle-ship-GUI) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require racket/base)
(require 2htdp/image)

;;------[EXPORTING]----------;;

(provide BATTLESHIP-GUI)
(provide BATTLESHIP-PLACE-GUI)
(provide PATROL-I)
(provide SUB-I)
(provide DESTROYER-I)
(provide BATTLESHIP-I)
(provide ACC-I)
(provide PATROL-I)
(provide PATROL-I)
(provide SS-PANE)
(provide SS-PANE-P)
(provide SS-PANE-S)
(provide SS-PANE-D)
(provide SS-PANE-B)
(provide SS-PANE-A)
(provide ST-SIZE-2)
(provide ST-SIZE-3)
(provide ST-SIZE-4)
(provide ST-SIZE-5)
(provide OCEAN-GRID)
(provide COORDINATE-BOX)
(provide COORDINATE-BOX-X)
(provide COORDINATE-BOX-Y)
(provide LOSE-SCREEN)
(provide WIN-SCREEN)
(provide TIE-SCREEN)


;;--------------------------;;




(define BACKGROUND (empty-scene 800 500))
(define TITLE (text "BATTLESHIP" 24 "BLACK"))



;;-------------[OCEAN GRID]-------------------------------;;

(define OCEAN-BG (rectangle 400 400 "solid" "lightblue"))
(define STRIP (beside (rectangle 50 50 "outline" "grey")
                      (rectangle 50 50 "outline" "grey")
                      (rectangle 50 50 "outline" "grey")
                      (rectangle 50 50 "outline" "grey")
                      (rectangle 50 50 "outline" "grey")
                      (rectangle 50 50 "outline" "grey")
                      (rectangle 50 50 "outline" "grey")
                      (rectangle 50 50 "outline" "grey")))
(define GRID (above STRIP
                    STRIP
                    STRIP
                    STRIP
                    STRIP
                    STRIP
                    STRIP
                    STRIP))


(define ALPHA-COORD (text "A             B             C               D                E              F             G             H " 12 "black"))
(define NUM-COORD (place-image (text "1" 12 "black") 7.5 25
                               (place-image (text "2" 12 "black") 7.5 75
                                            (place-image (text "3" 12 "black") 7.5 125
                                                         (place-image (text "4" 12 "black") 7.5 175 
                                                                      (place-image (text "5" 12 "black") 7.5 225
                                                                                   (place-image (text "6" 12 "black") 7.5 275
                                                                                                (place-image (text "7" 12 "black") 7.5 325
                                                                                                             (place-image (text "8" 12 "black") 7.5 375
                                                                                                                          (rectangle 15 400 "solid" "white"))))))))))

(define OCEAN-GRID (beside NUM-COORD (above ALPHA-COORD (overlay GRID OCEAN-BG))))

;;--------------------[COORDINATE SELECTION PANE]-------------;;

(define COORDINATE-BOX (rectangle 150 20 "outline" "black"))
(define HIT/MISS-LBOX (rectangle 50 200 "outline" "black"))
(define HIT (text "HITS" 12 "red"))
(define MISS (text "MISSES" 12 "blue"))
(define ENTER (text "Enter Coodinates:" 12 "black"))

;;-----------------[DIRECTIONS]---------------------------;;


(define D1 (text "Use the directional keys" 12 "black"))
(define D2 (text "to move the placment box" 12 "black"))
(define D3 (text "for the boat. To change" 12 "black"))
(define D4 (text "the type of boat simply press" 12 "black"))
(define D5 (text "the following:" 12 "black"))
(define D6 (text "p= patrol" 12 "black")) 
(define D7 (text "s= submarine" 12 "black"))
(define D8 (text "d= destroyer" 12 "black"))
(define D9 (text "b= battleship" 12 "black"))
(define D10 (text "a= aircraft-destroyer" 12 "black"))
(define D11 (text "Press enter to place the ship" 12 "black"))
(define D12 (text "Press \"o\" to change ship" 12 "black"))
(define D13 (text "orientation" 12 "black"))


(define SELECTION-D (above D1 D2 D3 D4 D5 D6 D7 D8 D9 D10 D11 D12 D13))

(define S (text "" 12 "black"))
(define B1 (text "Enter a coordinate into the" 12 "black")) 
(define B2 (text  "box to attack your opponent."  12 "black"))
(define B3 (text "If an orange box appears over" 12 "black"))
(define B4 (text "your boat, it means that you" 12 "black"))
(define B5 (text "have been hit." 12 "black"))

(define BATTLE-D (above B1 B2 B3 B4 B5))

(define DIRECTIONS (above (text "PLACEMENT DIRECTIONS" 12 "black") S SELECTION-D S (text "BATTLE DIRECTIONS" 12 "black") S BATTLE-D))

;;-----------------[SHIP COUNT PANE]---------------------------;;

(define SHIPS-REMAINING (text "Ships Remaining" 12 "black"))
(define SUBS(text "Submarines:" 12 "black"))
(define PATROLLER (text "Patrol Boat:" 12 "black"))
(define BATTLESHIP (text "Battleships:" 12 "black"))
(define DESTROYER (text "Destroyers:" 12 "black"))
(define ACC (text "Aircraft Carriers:" 12 "black"))
(define COUNT-BOX (rectangle 20 18 "outline" "black"))

;;----------------[DIRECTION BOX]-------------------------------;;

(define DIRECTION-BOX (rectangle 200 400 "outline" "black"))
(define DIRECTION-T (text "Directions" 12 "black"))
(define DIRECTION-PANE (above DIRECTION-T DIRECTION-BOX))

(define TITLE-X 280)
(define TITLE-Y 8)
(define COORD-PANE-X/Y 50)
(define COORDINATE-BOX-X 20)
(define COORDINATE-BOX-Y 70)
(define HIT-X 30)
(define MISS-X 122)
(define HIT/MISS-Y 110)
(define COUNTBOX-X 110)


(define BATTLESHIP-GUI (place-image/align TITLE TITLE-X TITLE-Y "left" "top" 
                       (place-image/align ENTER COORD-PANE-X/Y COORD-PANE-X/Y "left" "top" 
                       (place-image/align COORDINATE-BOX COORDINATE-BOX-X COORDINATE-BOX-Y "left" "top" 
                       (place-image/align HIT HIT-X HIT/MISS-Y "left" "top" 
                       (place-image/align MISS MISS-X HIT/MISS-Y "left" "top"
                       (place-image/align HIT/MISS-LBOX COORDINATE-BOX-X (+ 10 HIT/MISS-Y) "left" "top"
                       (place-image/align HIT/MISS-LBOX (+ HIT-X 90) 120 "left" "top" 
                       (place-image/align SHIPS-REMAINING 50 335 "left" "top"
                       (place-image/align PATROLLER COORDINATE-BOX-X 365 "left" "top"
                       (place-image/align COUNT-BOX COUNTBOX-X 360 "left" "top"
                       (place-image/align SUBS COORDINATE-BOX-X 385 "left" "top"
                       (place-image/align COUNT-BOX COUNTBOX-X 380 "left" "top"
                       (place-image/align BATTLESHIP COORDINATE-BOX-X 405 "left" "top"
                       (place-image/align COUNT-BOX COUNTBOX-X 400 "left" "top"
                       (place-image/align DESTROYER COORDINATE-BOX-X 425 "left" "top"
                       (place-image/align COUNT-BOX COUNTBOX-X 420 "left" "top"
                       (place-image/align ACC COORDINATE-BOX-X 445 "left" "top"
                       (place-image/align COUNT-BOX COUNTBOX-X 440 "left" "top"
                       (place-image/align DIRECTION-PANE 610 50 "left" "top" 
                       (place-image/align DIRECTIONS 620 70 "left" "top" 
                       (place-image/align OCEAN-GRID 190 50 "left" "top" 
                        BACKGROUND)))))))))))))))))))))) 



;;--------------[SELECTION PANE]--------------------;;



(define SELECT (text "Place your boats" 12 "black"))


(define BATTLESHIP-PLACE-GUI (place-image/align TITLE TITLE-X TITLE-Y "left" "top"
                        (place-image/align SELECT COORD-PANE-X/Y COORD-PANE-X/Y "left" "top"                         
                       (place-image/align SHIPS-REMAINING 50 335 "left" "top"
                       (place-image/align PATROLLER COORDINATE-BOX-X 365 "left" "top"
                       (place-image/align COUNT-BOX COUNTBOX-X 360 "left" "top"
                       (place-image/align SUBS COORDINATE-BOX-X 385 "left" "top"
                       (place-image/align COUNT-BOX COUNTBOX-X 380 "left" "top"
                       (place-image/align BATTLESHIP COORDINATE-BOX-X 405 "left" "top"
                       (place-image/align COUNT-BOX COUNTBOX-X 400 "left" "top"
                       (place-image/align DESTROYER COORDINATE-BOX-X 425 "left" "top"
                       (place-image/align COUNT-BOX COUNTBOX-X 420 "left" "top"
                       (place-image/align ACC COORDINATE-BOX-X 445 "left" "top"
                       (place-image/align COUNT-BOX COUNTBOX-X 440 "left" "top"
                       (place-image/align DIRECTION-PANE 610 50 "left" "top"
                       (place-image/align DIRECTIONS 620 70 "left" "top" 
                       (place-image/align OCEAN-GRID 190 50 "left" "top" 
                        BACKGROUND)))))))))))))))))

;;----------[BOATS]----------------------;;



(define PATROL-I (place-image (overlay (ellipse  7 12 "outline" "grey")
                                       (rectangle  25 25 "outline" "grey")
                                       (ellipse  40 90 "outline" "grey")) 25 50
                                                                          (ellipse  50 100 "solid" "Silver")))


(define SUB-I (place-image (overlay (ellipse  13 25 "outline" "grey")
                                    (ellipse  25 50 "outline" "grey")) 25 100
                                                                       (ellipse  50 150 "solid" "Silver")))
(define DESTROYER-I (place-image (overlay (ellipse  13 25 "outline" "grey")
                                          (ellipse  25 50 "outline" "grey")) 25 75
                                                                             (place-image (ellipse  40 130 "outline" "grey") 25 75
                                                                                          (ellipse  50 150 "solid" "Silver"))))

(define BATTLESHIP-I (place-image (overlay (ellipse  13 25 "outline" "grey")
                                           (ellipse  25 50 "outline" "grey")) 25 50
                                                                              (place-image (overlay (ellipse  13 25 "outline" "grey")
                                                                                                    (ellipse  25 75 "outline" "grey")) 25 150
                                                                                                                                       (place-image (ellipse  40 190 "outline" "grey") 25 100
                                                                                                                                                    (ellipse  50 200 "solid" "Silver")))))




(define ACC-I (place-image (overlay (ellipse  13 25 "outline" "grey")
                                    (rectangle 25 75 "outline" "grey")) 25 160
                                                                        (overlay (rectangle  35 150 "outline" "grey")
                                                                                 (ellipse  50 250 "solid" "Silver"))))



;;-----------[SHIP SELECTION PANES]--------------;;



(define SS-PANE (scale 1/2 (rotate 270 (beside PATROL-I
                                               SUB-I
                                               DESTROYER-I
                                               BATTLESHIP-I
                                               ACC-I))))

(define SS-PANE-P (scale 1/2 (rotate 270 (beside (overlay PATROL-I (rectangle 50 250 "outline" "orangered")) 
                                               SUB-I
                                               DESTROYER-I
                                               BATTLESHIP-I
                                               ACC-I))))
(define SS-PANE-S (scale 1/2 (rotate 270 (beside PATROL-I 
                                               (overlay SUB-I (rectangle 50 250 "outline" "orangered"))
                                               DESTROYER-I
                                               BATTLESHIP-I
                                               ACC-I))))
(define SS-PANE-D (scale 1/2 (rotate 270 (beside PATROL-I
                                               SUB-I
                                               (overlay DESTROYER-I (rectangle 50 250 "outline" "orangered"))
                                               BATTLESHIP-I
                                               ACC-I))))
(define SS-PANE-B (scale 1/2 (rotate 270 (beside PATROL-I
                                               SUB-I
                                               DESTROYER-I
                                               (overlay BATTLESHIP-I (rectangle 50 250 "outline" "orangered"))
                                               ACC-I))))
(define SS-PANE-A (scale 1/2 (rotate 270 (beside PATROL-I
                                               SUB-I
                                               DESTROYER-I
                                               BATTLESHIP-I
                                               (overlay ACC-I (rectangle 50 250 "outline" "orangered"))))))


;;---------[SHIP PLACEMENT TEMPLATES]------------;;



(define ST-SIZE-2 (rectangle 50 100 "solid" "orangered"))

(define ST-SIZE-3 (rectangle 50 150 "solid" "orangered"))

(define ST-SIZE-4 (rectangle 50 200 "solid" "orangered"))

(define ST-SIZE-5 (rectangle 50 250 "solid" "orangered"))

;;---------[WIN/LOSE]------------;;

(define WIN-SCREEN (overlay (text "YOU WON THE BATTLE!!!" 36 "green") BACKGROUND))
(define LOSE-SCREEN (overlay (text "YOU LOST THE BATTLE." 36 "red") BACKGROUND))
(define TIE-SCREEN (overlay (text "TIE...Thats not Fun." 36 "blue") BACKGROUND))
  
  
 