;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Battleship) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; Project 2:  Battleship
;; By: Shi Johnson-Bey and Shivani Murali


(require "battle-ship-GUI.rkt")
(require 2htdp/image)
(require 2htdp/universe)



;;----------[DATA DEFINITIONS]----------------------;;



;; a coordinate is a 2 character alpha-numeric pair containing one capital letter and a number
;; -- All cominations of the letters A-H and numbers 1-8 
;; -- example: "A1" or "C7"
;; -- range: "A1" - "H8"


;; a list of coordinate LoC is either
;; -- empty
;; -- (cons coordinate LoC)


;; a Patrol is a struct
(define-struct Patrol (coordinates))
;; interp of fields
;;  coordinates [listof coordinate]: the coordinates that the ship takes up (max-length: 2)
;; make-Patrol: [listof coordinate] -> Patrol
(define EX-PATROL1 (make-Patrol (list "A1" "A2")))
(define EX-PATROL2 (make-Patrol (list "B1" "B2")))
#|
(define (Patrol-fun aShip)
     ...(Patrol-coordinates)...) ; [listof coordinate]
)
|#


;; a Sub is a struct
(define-struct Sub (coordinates))
;; interp of fields
;;  coordinates [listof coordinate]: the coordinates that the ship takes up (max-length: 3)
;; make-Sub: [listof coordinate] -> Sub 
(define EX-SUB1 (make-Sub (list "C1" "C2" "C3")))
#|
(define (Sub-fun aShip)
     ...(Sub-coordinates)...) ; [listof coordinate]
)
|#


;; a Destroyer is a struct
(define-struct Destroyer (coordinates))
;; interp of fields
;;  coordinates [listof coordinate]: the coordinates that the ship takes up (max-length: 3)
;; make-Destroyer: [listof coordinate] -> Destroyer
(define EX-DESTROYER1 (make-Destroyer (list "D1" "D2" "D3")))
#|
(define (Destroyer-fun aDestroyer)
     ...(Destroyer-coordinates)...) ; [listof coordinate]
)
|#


;; a BattleShip is a struct
(define-struct Battleship (coordinates))
;; interp of fields
;;  coordinates [listof coordinate]: the coordinates that the ship takes up (max-length: 4)
;; make-Battleship: [listof coordinate] -> Battleship
(define EX-BATTLESHIP1 (make-Battleship (list "E1" "E2" "E3" "E4")))
#|
(define (Battleship-fun aBattleship)
     ...(Battleship-coordinates)...) ; [listof coordinate]
)
|#


;; a Aircraft-Car is a struct
(define-struct Aircraft-Car (coordinates))
;; interp of fields
;;  coordinates [listof coordinate]: the coordinates that the ship takes up (max-length: 5)
;; make-Aircraft-Car: [listof coordinate] -> Aircraft-Car
(define EX-AIRCRAFT-CAR1 (make-Aircraft-Car (list "F1" "F2" "F3" "F4" "F5")))
#|
(define (Aircraft-Car-fun aBattleship)
     ...(Aircraft-Car-coordinates)...) ; [listof coordinate]
)
|#


;; a Ship is either
;; -- Sub
;; -- Patrol
;; -- Destroyer
;; -- Battleship
;; -- Aircraft-Car
#|
(define (Ship-fun aShip)
  (cond [(Patrol? aShip) ...]
        [(Sub? aShip) ...]
        [(Destroyer? aShip) ...]
        [(Battleship? aShip) ...]
        [(Aircraft-Car? aShip) ...])
)
|#


;; a list of ship LoS is either
;; -- empty
;; -- (cons ship LoS)
(define A-SHIPS (list EX-AIRCRAFT-CAR1 
                      EX-BATTLESHIP1 
                      EX-DESTROYER1 
                      EX-SUB1 
                      EX-SUB1 
                      EX-PATROL1 
                      EX-PATROL1 
                      EX-PATROL1))
#|
(define (LoS-fun aLoS)
  (cond [(empty? aLoS) ... ]
        [(cons? aLoS) 
           (first aLoS)
           (LoS-fun (rest aLoS))])
)
|#


;; a player is a struct
(define-struct Player (ships unplaced-ships ship-selection hits misses))
;; interp of fields
;;  ships [listof Ship]: List of the players in-game ships
;;  unplaced-ships [listof Ship]: List of the players remaining ships for placing
;;  ship-selection [Ship]: The current selected ship for placing
;;  hits [listof coordinate]: List of Corrdinates that were successful hits on the opponent's ships
;;  misses [listof coordinate]: List of coordinates that were missed shots at opponent's ships
;; make-Player: [listof Ship] [listof coordinate] [listof coordinate] -> Player
(define EX-PLAYER1 (make-Player (list EX-SUB1) empty EX-SUB1 (list "H8") (list "F8")))
(define EX-PLAYER2 (make-Player (list EX-SUB1) empty EX-SUB1 empty empty))
(define PLAYER1 (make-Player empty A-SHIPS EX-SUB1 empty empty))
(define PLAYER2 (make-Player empty A-SHIPS EX-SUB1 empty empty))
#|
(define (Player-fun aPlayer)
     ...(Player-ships aPlayer)... ; [listof Ship]
     ...(Player-hits aPlayer)... ; [listof coordinate]
     ...(Player-misses aPlayer)... ; [listof coordinate]
)
|#



;;-----------------------[CPU-PLAYERS]---------------------------


(define COORDINATES (list "A1" "A2" "A3" "A4" "A5" "A6" "A7" "A8"
                          "B1" "B2" "B3" "B4" "B5" "B6" "B7" "B8"
                          "C1" "C2" "C3" "C4" "C5" "C6" "C7" "C8"
                          "D1" "D2" "D3" "D4" "D5" "D6" "D7" "D8"
                          "E1" "E2" "E3" "E4" "E5" "E6" "E7" "E8"
                          "F1" "F2" "F3" "F4" "F5" "F6" "F7" "F8"
                          "G1" "G2" "G3" "G4" "G5" "G6" "G7" "G8"
                          "H1" "H2" "H3" "H4" "H5" "H6" "H7" "H8"))
(define USED empty)
(define P-USED empty)


;; attack: nothing -> Coordinate
;; Consumes: nothing
;; Produces: Coordinates
(define (attack)
  (local[(define coord (list-ref COORDINATES (random (length COORDINATES))))]
    (cond[(ormap (lambda (x) (string=? coord x)) USED)
          (attack)]
         [else (begin
                 (set! USED (cons coord USED))
                 coord)])))

(define CPU1 (make-Player
              (list
               (make-Patrol (list "D8"))
               (make-Patrol (list "B8"))
               (make-Battleship (list "G7" "G6" "G5" "G4"))
               (make-Destroyer (list "D6" "D5" "D4"))
               (make-Aircraft-Car (list "H2" "G2" "F2" "E2" "D2"))
               (make-Sub (list "H8" "H7"))
               (make-Sub (list "A4" "A3" "A2")))
              empty
              (make-Patrol (list "A1" "A2"))
              empty
              empty))

(define CPU2 (make-Player
              (list
               (make-Patrol (list "E7" "D7"))
               (make-Patrol (list "F5" "F4"))
               (make-Patrol (list "C5" "C4"))
               (make-Battleship (list "A7" "A6" "A5" "A4"))
               (make-Aircraft-Car (list "H6" "H5" "H4" "H3" "H2"))
               (make-Sub (list "C8" "B8" "A8"))
               (make-Sub (list "C1" "B1" "A1")))
              empty
              (make-Patrol (list "A1" "B2"))
              empty
              empty))

(define CPU3   (make-Player
                (list
                 (make-Patrol (list "E4" "D4"))
                 (make-Patrol (list "C5" "C4"))
                 (make-Patrol (list "F5" "F4"))
                 (make-Destroyer (list "G7" "G6" "G5"))
                 (make-Battleship (list "F7" "E7" "D7" "C7"))
                 (make-Aircraft-Car (list "G2" "F2" "E2" "D2" "C2"))
                 (make-Sub (list "B7" "B6" "B5"))
                 (make-Sub (list "B4" "B3" "B2")))
                empty
                (make-Patrol (list "E4" "D4"))
                empty
                empty))


(define CPU-PLAYERS (list CPU1 CPU2 CPU3))



;;---------------------------------------------------------------------

;; a list of player LoP is either
;; -- empty
;; -- (cons Player LoP)


;; a binary-number is either
;; --0
;; --1

;; a World is a struct
(define-struct World (player-turn players))
;; interp of fields
;;  player-turn [binary-number]: indicates which players turn it is 0 or 1
;;  players [listof Player]: a list of length 2 containing 2 players
;; make-World: binary-number [listof Player] -> World
(define EX-WORLD1 (make-World 0 (list EX-PLAYER1 EX-PLAYER2)))
(define GAME-WORLD (make-World 0 (list PLAYER1 PLAYER2)))
(define GAME-WORLD2 (make-World 0 (list PLAYER1 (make-Player empty empty EX-SUB1 empty empty))))
(define GAME-WORLDF (make-World 0 (list PLAYER1 (list-ref CPU-PLAYERS (random (length CPU-PLAYERS))))))
#|
(define (World-fun aWorld)
     ...(World-player-turn aWorld)...) ; binary-number
     ...(World-players aWorld)...) ; [listof Player]
)
|#



;;-----------------------[CONSTANTS]---------------------------


(define GAME-GUI BATTLESHIP-GUI)
(define PLACE-BOX ST-SIZE-3)
;(define DAMAGE-BOX (rectangle 50 50 "solid" "orange"))
(define HIT-CIRCLE (circle 12.5 "solid" "red"))
(define MISS-CIRCLE (circle 12.5 "solid" "white"))




;;------------------------[FUNCTIONS]--------------------------


;; game-begin?: [listof Ship] [listof Ship] -> boolean
;; Consumes: two [listof Ship]
;; Produces: true if both lists are empty, false otherwise
(define (game-begin? aLoS1 aLoS2)
  (and (empty? aLoS1) (empty? aLoS2)))

(check-expect (game-begin? (list EX-DESTROYER1 EX-SUB1) empty) false)
(check-expect (game-begin? empty empty) true)
(check-expect (game-begin? (list EX-DESTROYER1 EX-SUB1) (list EX-DESTROYER1 EX-SUB1)) false)

;; game-over?: aWorld -> boolean
;; Consumes: aWorld
;; Produces: true either one of the players has no ships or unplaced-ships.
(define (game-over? aWorld)
  (or (and (empty? (Player-unplaced-ships (first (World-players aWorld))))
           (empty? (Player-ships (first (World-players aWorld)))))
      (and (empty? (Player-unplaced-ships (second (World-players aWorld))))
           (empty? (Player-ships (second (World-players aWorld)))))))

;; win/lose-screen: World -> image
;; Consumes: a World
;; Produces: an Image of either the win or lose screen
(define (win/lose-screen aWorld)
  (cond [(and (empty? (Player-unplaced-ships (first (World-players aWorld))))
              (empty? (Player-ships (first (World-players aWorld)))))
         LOSE-SCREEN]
        [(and (empty? (Player-unplaced-ships (second (World-players aWorld))))
              (empty? (Player-ships (second (World-players aWorld)))))
         WIN-SCREEN]
        [else TIE-SCREEN]))



;; get-coordinates: Ship -> [listof Coordinate]
;; Consumes: a Ship
;; Produces: the [listof Coordinate] of the ship
(define (get-coordinates aShip)
  (cond [(Patrol? aShip) (Patrol-coordinates aShip)]
        [(Sub? aShip) (Sub-coordinates aShip)]
        [(Destroyer? aShip) (Destroyer-coordinates aShip)]
        [(Battleship? aShip) (Battleship-coordinates aShip)]
        [(Aircraft-Car? aShip) (Aircraft-Car-coordinates aShip)]))

(check-expect (get-coordinates EX-PATROL1) (list "A1" "A2"))
(check-expect (get-coordinates EX-SUB1) (list "C1" "C2" "C3"))
(check-expect (get-coordinates EX-DESTROYER1) (list "D1" "D2" "D3"))
(check-expect (get-coordinates EX-BATTLESHIP1) (list "E1" "E2" "E3" "E4"))
(check-expect (get-coordinates EX-AIRCRAFT-CAR1) (list "F1" "F2" "F3" "F4" "F5"))


;; getall-coords: Player -> [llistof Coordinate]
;; Consumes: a player
;; Produces: a [listof Coordinate] containing the coordinates of all the ships
(define (getall-coords aPlayer)
  (foldr (lambda (x list) (append (get-coordinates x) list)) empty (Player-ships aPlayer)))

(check-expect (getall-coords (make-Player (list EX-SUB1 EX-DESTROYER1) empty EX-DESTROYER1 empty empty))
              (list "C1" "C2" "C3" "D1" "D2" "D3"))

;; can-place?: Player -> boolean
;; consumes: aPlayer
;; Produces: true if the player may place a ship
(define (can-place? aPlayer)
  (local[(define prex (getall-coords aPlayer))
         (define (helper acoord)
           (ormap (lambda (x) (string=? acoord x)) prex))]
    
    (cond[(ormap helper (determine-coordinates)) false]
         [else true])))




;; hit?: Coordinate [listof Ship] -> boolean
;; Consumes: a coordinate and a [listof Ship]
;; Produces: true if the given coordinate matches any of the coordinates of any of the ships
;;           false otherwise
(define (hit? aCoord aLoS)          
  (ormap (lambda (aship) (ormap (lambda (aCoord1) (string=? aCoord aCoord1)) (get-coordinates aship))) aLoS))

(check-expect (hit? "H8" (list EX-PATROL1 EX-PATROL1 EX-BATTLESHIP1)) false)
(check-expect (hit? "A1" (list EX-PATROL1 EX-PATROL1 EX-BATTLESHIP1)) true)


;; damage-ship: Coordinate [listof Ship] -> [listof Ship]
;; Consumes: a coordinate and a [listof Ship]
;; Produces: [listof Ship] with the coordinate of the hit ship removed
(define (damage-ship aCoord aLoS)
  (local[(define (damage aShip)
           (cond
             [(Patrol? aShip) 
              (make-Patrol (filter (lambda (x) (not (string=? aCoord x))) (Patrol-coordinates aShip)))]
             [(Sub? aShip) 
              (make-Sub (filter (lambda (x) (not (string=? aCoord x))) (Sub-coordinates aShip)))]
             [(Destroyer? aShip) 
              (make-Destroyer (filter (lambda (x) (not (string=? aCoord x))) (Destroyer-coordinates aShip)))]
             [(Battleship? aShip)
              (make-Battleship (filter (lambda (x) (not (string=? aCoord x))) (Battleship-coordinates aShip)))]
             [(Aircraft-Car? aShip) 
              (make-Aircraft-Car (filter (lambda (x) (not (string=? aCoord x))) (Aircraft-Car-coordinates aShip)))]))]              
    (map damage aLoS)))


(check-expect (get-coordinates (first (damage-ship "A1" (list EX-PATROL1 
                                                              EX-SUB1
                                                              EX-DESTROYER1
                                                              EX-BATTLESHIP1
                                                              EX-AIRCRAFT-CAR1)))) 
              (list "A2"))
(check-expect (get-coordinates (first (damage-ship "H8" (list EX-PATROL1 EX-PATROL1 EX-BATTLESHIP1)))) (list "A1" "A2"))


;; sunk?: Ship -> boolean
;; Consumes: a Ship
;; Produces: true if the ship's [listof Coordinate] is empty
;;           false otherwise
(define (sunk? aShip)
  (empty? (get-coordinates aShip)))

(check-expect (sunk? (make-Sub empty)) true)
(check-expect (sunk? EX-SUB1) false)


;; remove-ship: Player -> [listof Ship]
;; Consumes: a Ship and a [listof Ship]
;; Produces: a [listof Ship] with one elemt of that type removed
(define (remove-ship aPlayer)
  (local[(define count 0)
         (define (helper ship1 list)
           (cond[(and (equal? (Player-ship-selection aPlayer) ship1)(= 0 count))
                 (begin
                   (set! count 1)
                   list)]
                [else
                 (cons ship1 list)]))]
    (foldr helper empty (Player-unplaced-ships aPlayer))))

(check-expect (remove-ship (make-Player empty
                                        (list EX-PATROL1 EX-PATROL2 EX-SUB1 EX-SUB1)
                                        EX-SUB1
                                        empty
                                        empty)) (list EX-PATROL1 EX-PATROL2 EX-SUB1))

;; get-y: String -> number
;; Consumes: a single chatracter string
;; Produces: an number representing an pixel value
(define (get-y aString)
  (cond[(string=? "1" aString) 62]
       [(string=? "2" aString) 112]
       [(string=? "3" aString) 162]
       [(string=? "4" aString) 212]
       [(string=? "5" aString) 262]
       [(string=? "6" aString) 312]
       [(string=? "7" aString) 362]
       [(string=? "8" aString) 412]))

(check-expect (get-y "1") 62)
(check-expect (get-y "2") 112)
(check-expect (get-y "3") 162)
(check-expect (get-y "4") 212)
(check-expect (get-y "5") 262)
(check-expect (get-y "6") 312)
(check-expect (get-y "7") 362)
(check-expect (get-y "8") 412)


;; get-x: String -> number
;; Consumes: a single chatracter string
;; Produces: an number representing an pixel value
(define (get-x aString)
  (cond[(string=? "A" aString) 205]
       [(string=? "B" aString) 255]
       [(string=? "C" aString) 305]
       [(string=? "D" aString) 355]
       [(string=? "E" aString) 405]
       [(string=? "F" aString) 455]
       [(string=? "G" aString) 505]
       [(string=? "H" aString) 555]))

(check-expect (get-x "A") 205)
(check-expect (get-x "B") 255)
(check-expect (get-x "C") 305)
(check-expect (get-x "D") 355)
(check-expect (get-x "E") 405)
(check-expect (get-x "F") 455)
(check-expect (get-x "G") 505)
(check-expect (get-x "H") 555)


;; place-hit-circle: Coordinate -> nothing
;; Consumes: a Coordinate String
;; Produces: nothing, but overlays a red circle where the ship is damaged
(define (place-hit-circle aCoord)
  (set! GAME-GUI (place-image HIT-CIRCLE
                              (+ 25 (get-x (substring aCoord 0 1)))
                              (+ 25 (get-y (substring aCoord 1 2)))
                              
                              GAME-GUI)))

;; place-miss-circle: Coordinate -> nothing
;; Consumes: a Coordinate String
;; Produces: nothing, but overlays a red circle where the ship is damaged
(define (place-miss-circle aCoord)
  (set! GAME-GUI (place-image MISS-CIRCLE
                              (+ 25 (get-x (substring aCoord 0 1)))
                              (+ 25 (get-y (substring aCoord 1 2)))
                              
                              GAME-GUI)))



;; ships-left?: Player -> boolean
;; Consumes: a Player
;; Produces: true if the player can place a Ship
;;           false otherwise
(define (ships-left? aPlayer)
  (local[(define select (Player-ship-selection aPlayer))]
    (cond [(and (Patrol? select) (> (patrol-count (Player-unplaced-ships aPlayer)) 0)) true]
          [(and (Sub? select) (> (sub-count (Player-unplaced-ships aPlayer)) 0)) true]
          [(and (Destroyer? select) (> (destroyer-count (Player-unplaced-ships aPlayer)) 0)) true]
          [(and (Battleship? select) (> (battleship-count (Player-unplaced-ships aPlayer)) 0)) true]
          [(and (Aircraft-Car? select) (> (aircraft-count (Player-unplaced-ships aPlayer)) 0)) true]
          [else false])))



;; do-until: (none -> boolean) (none -> void) (none -> X) -> X
;; while (P?) is false, do (BODY), finally return (RESULT)
;;   P? (none -> boolean) : predicate that when true will cause loop to stop and return (RESULT).
;;   BODY (none -> void)  : a mutating function that is called once every time P? is evaluated
;;   RESULT (none -> X)   : a function that is called ONCE, when the P? is true, and returns the desired result
;; NOTE HOW LITTLE HELP THE SIGNATURE GIVES YOU!!!!!
(define (do-until P? BODY RESULT)
  (if (P?)
      (RESULT)
      (begin
        (BODY)
        (do-until P? BODY RESULT))))


;;------------------------[COORDINATE BOX (STEB) ]--------------------------

(define STEBH (image-height COORDINATE-BOX))
(define STEBW (image-width COORDINATE-BOX))

;; A positive-number is a number >= 0
;;A STEB is a struct
(define-struct STEB (text height width cursor))
;; A STEB is (make-steb string positive-number positive-number positive-number)
;; interp of fields:
;;  text [string]: text in the box
;;  height [number]: height of the box
;;  width: [number]: width of the box
;;  cursor [number]: location of cursor. Cursor location may not exceed length of string.
;; make-steb: string number number number --> STEB
(define COORD-STEB1 (make-STEB "Enter a Coordinate" STEBH STEBW 0))
(define STEB1 (make-STEB "Hello World" 50 400 7))
(define STEB2 (make-STEB "Dank" 50 400 4))
(define STEB3 (make-STEB "Practice" 50 400 0))
#| 
(define (STEB-fun aSTEB)
    ... (STEB-text aSTEB) ... ;String
    ... (STEB-height aSTEB) ... ;Number
    ... (STEB-width aSTEB) ... ;Number
    ... (STEB-cursor aSTEB) ... ;Number
)
|#




;; positive-number is a nummber >= 0
;; update-steb-cloc: STEB positive-number -> STEB
;; Consumes: a STEB and a positive-number representing a new cursor position
;; Produces: produces a new STEB with an updated cursor position
(define (update-steb-cloc aSTEB newcursor)
  (make-STEB (STEB-text aSTEB) (STEB-height aSTEB) (STEB-width aSTEB) newcursor ))

(check-expect (STEB-cursor (update-steb-cloc STEB1  3)) 3)



;; move-cursor-left: STEB -> STEB
;; Consumes: a STEB
;; Produces: a new STEB with the cursor one character to the left (if cursor position is greater than 0)
(define (move-cursor-left)
  (cond[(> (STEB-cursor COORD-STEB1) 0) 
        (begin
          (set! COORD-STEB1
                (make-STEB (STEB-text COORD-STEB1) (STEB-height COORD-STEB1) (STEB-width COORD-STEB1) (- (STEB-cursor COORD-STEB1) 1 ) ))
          )]
       [else COORD-STEB1]))

#|
(check-expect (STEB-cursor (move-cursor-left STEB1)) 6)
(check-expect (STEB-cursor (move-cursor-left STEB3)) 0) 
|#


;; move-cursor-right: STEB -> STEB
;; Consumes: a STEB
;; Produces: new STEB with the cursor one character to the right
(define (move-cursor-right)
  (cond[(< (STEB-cursor COORD-STEB1) (string-length (STEB-text COORD-STEB1)))
        (begin
          (set! COORD-STEB1
                (make-STEB (STEB-text COORD-STEB1) (STEB-height COORD-STEB1) (STEB-width COORD-STEB1) (+ (STEB-cursor COORD-STEB1) 1 )))
          )]
       [else COORD-STEB1]))

#|
(check-expect (STEB-cursor (move-cursor-right STEB1)) 8)
(check-expect (STEB-cursor (move-cursor-right STEB2)) 4)
|#



;; delete-char: STEB -> STEB
;; Consumes: a STEB 
;; Produces: new STEB without the character to the left of the cursor. 
;;           If cursor is at position zero delete-char does nothing.
(define (delete-char)
  (begin
    (set! COORD-STEB1
          (make-STEB (cond [(= 0 (STEB-cursor COORD-STEB1))  (STEB-text COORD-STEB1)]
                           [(< (STEB-cursor COORD-STEB1) (string-length (STEB-text COORD-STEB1))) 
                            (string-append (substring (STEB-text COORD-STEB1) 0 (STEB-cursor COORD-STEB1)) 
                                           (substring (STEB-text COORD-STEB1) (+ 1 (STEB-cursor COORD-STEB1))))]
                           [else (substring (STEB-text COORD-STEB1) 0 (- (STEB-cursor COORD-STEB1) 1))])
                     (STEB-height COORD-STEB1) 
                     (STEB-width COORD-STEB1) 
                     (cond [(= 0 (STEB-cursor COORD-STEB1)) 0]
                           [else (- (STEB-cursor COORD-STEB1) 1)])))
    ))

#|
(check-expect (STEB-text (delete-char STEB2)) "Dan")
(check-expect (STEB-text (delete-char STEB3)) "Practice")
(check-expect (STEB-text (delete-char STEB1)) "Hello Wrld")
|#



;; insert-char: STEB string -> STEB
;; Consumes: A STEB and a single character
;; Produces: A new STEB with the character to the left of the cursor
(define (insert-char char)
  (begin
    (set! COORD-STEB1  
          (make-STEB (cond[(= 0 (STEB-cursor COORD-STEB1)) 
                           (string-append char (STEB-text COORD-STEB1))]
                          [(> (string-length (STEB-text COORD-STEB1)) (STEB-cursor COORD-STEB1)) 
                           (string-append (substring (STEB-text COORD-STEB1) 0 (STEB-cursor COORD-STEB1)) 
                                          char 
                                          (substring (STEB-text COORD-STEB1) (STEB-cursor COORD-STEB1)))]
                          [(= (string-length (STEB-text COORD-STEB1)) (STEB-cursor COORD-STEB1)) 
                           (string-append (STEB-text COORD-STEB1) char)]) 
                     (STEB-height COORD-STEB1) 
                     (STEB-width COORD-STEB1)  
                     (+ (STEB-cursor COORD-STEB1) 1)))
    ))




(define CURSOR (text "|" 12 "red"))
;; render-string-with-cursor: STEB -> Image
;; Consumes: a STEB
;; Produces: Image with the "|" in the correct position within the text
(define (render-string-with-cursor aSTEB)
  (cond[(= 0 (STEB-cursor aSTEB)) 
        (beside CURSOR (text (STEB-text aSTEB) 12 "black"))]
       [(> (string-length (STEB-text aSTEB)) (STEB-cursor aSTEB)) 
        (beside (text (substring (STEB-text aSTEB) 0 (STEB-cursor aSTEB)) 12 "black") 
                CURSOR 
                (text (substring (STEB-text aSTEB) (STEB-cursor aSTEB)) 12 "black"))]
       [(= (string-length (STEB-text aSTEB)) (STEB-cursor aSTEB)) 
        (beside (text (STEB-text aSTEB) 12 "black") CURSOR)] 
       ))

(check-expect (render-string-with-cursor STEB1) (beside (text (substring "Hello World" 0 7) 12 "black") 
                                                        CURSOR 
                                                        (text (substring "Hello World" 7) 12 "black")))
(check-expect (render-string-with-cursor STEB2) (beside (text "Dank" 12 "black") CURSOR))
(check-expect (render-string-with-cursor STEB3) (beside CURSOR (text "Practice" 12 "black")))


;; render-steb: STEB -> Worldstate
;; Consumes: a STEB
;; Produces: a Worldstate showing the steb
(define (render-steb aSTEB)
  (place-image (render-string-with-cursor aSTEB) 
               (* .5 STEBW) (* .5 STEBH) 
               (empty-scene (STEB-width aSTEB) (STEB-height aSTEB))))

(check-expect (render-steb STEB1) 
              (place-image (render-string-with-cursor STEB1) 
                           (* .5 STEBW) (* .5 STEBH) 
                           (empty-scene (STEB-width STEB1) (STEB-height STEB1))))
(check-expect (render-steb STEB2) 
              (place-image (render-string-with-cursor STEB2) 
                           (* .5 STEBW) (* .5 STEBH) 
                           (empty-scene (STEB-width STEB2) (STEB-height STEB2))))
(check-expect (render-steb STEB3) 
              (place-image (render-string-with-cursor STEB3) 
                           (* .5 STEBW) (* .5 STEBH) (empty-scene 
                                                      (STEB-width STEB3) (STEB-height STEB3)))) 




;; an STEBKeyEvent is either
;; -- "left": move cursor 1 left, unless already at start of string
;; -- "right": move cursor 1 right, unless already at end of string
;; -- "\b" [backspace]: delete the character to the left of the cursor
;; -- any string of length 1: insert string at the current cursor location.
;; -- otherwise ignore the key

;; battle-keyhandler: aWorld STEBKeyEvent -> aWorld
;; Consumes: aWorld and a keyEvent
;; Produces: a new World based on the KeyEvent
(define (battle-keyhandler aWorld STEBKeyEvent) 
  (local[(define cpu-attack (attack))]
    (cond
      [(key=? STEBKeyEvent "left") (begin (move-cursor-left) aWorld)]
      [(key=? STEBKeyEvent "right") (begin (move-cursor-right) aWorld)]
      [(key=? STEBKeyEvent "\b") (begin (delete-char)  aWorld)]
      [(key=? STEBKeyEvent "\r")
       (cond[(ormap (lambda (x) (string=? x (STEB-text COORD-STEB1))) COORDINATES)
             (cond[(not (ormap (lambda (x) (string=? x (STEB-text COORD-STEB1))) P-USED))
                   (begin
                     (set! P-USED (cons (STEB-text COORD-STEB1) P-USED))
                     (cond[(hit? (STEB-text COORD-STEB1) (Player-ships (second (World-players aWorld))))
                           (cond[(hit? cpu-attack (Player-ships (first (World-players aWorld))))
                                 ;; Coordinate -> World
                                 (begin
                                   (place-hit-circle cpu-attack)
                                   (make-World 
                                    (World-player-turn aWorld)
                                    (list (make-Player (damage-ship cpu-attack (Player-ships (first (World-players aWorld))))
                                                       (Player-unplaced-ships (first (World-players aWorld)))
                                                       (Player-ship-selection (first (World-players aWorld)))
                                                       (cons (STEB-text COORD-STEB1) (Player-hits (first (World-players aWorld))))
                                                       (Player-misses (first (World-players aWorld))))
                                          (make-Player (damage-ship (STEB-text COORD-STEB1) (Player-ships (second (World-players aWorld))))
                                                       (Player-unplaced-ships (second (World-players aWorld)))
                                                       (Player-ship-selection (second (World-players aWorld)))
                                                       (cons cpu-attack (Player-hits (second (World-players aWorld))))
                                                       (Player-misses (second (World-players aWorld)))))))]
                                [else
                                 (make-World 
                                  (World-player-turn aWorld)
                                  (list (make-Player (Player-ships (first (World-players aWorld)))
                                                     (Player-unplaced-ships (first (World-players aWorld)))
                                                     (Player-ship-selection (first (World-players aWorld)))
                                                     (cons (STEB-text COORD-STEB1) (Player-hits (first (World-players aWorld))))
                                                     (Player-misses (first (World-players aWorld))))
                                        (make-Player (damage-ship (STEB-text COORD-STEB1) (Player-ships (second (World-players aWorld))))
                                                     (Player-unplaced-ships (second (World-players aWorld)))
                                                     (Player-ship-selection (second (World-players aWorld)))
                                                     (Player-hits (second (World-players aWorld)))
                                                     (cons cpu-attack (Player-misses (second (World-players aWorld)))))))])]
                          [else
                           (cond[(hit? cpu-attack (Player-ships (first (World-players aWorld))))
                                 (begin
                                   (place-hit-circle cpu-attack)
                                   (place-miss-circle (STEB-text COORD-STEB1))
                                   (make-World 
                                    (World-player-turn aWorld)
                                    (list (make-Player (damage-ship cpu-attack (Player-ships (first (World-players aWorld))))
                                                       (Player-unplaced-ships (first (World-players aWorld)))
                                                       (Player-ship-selection (first (World-players aWorld)))
                                                       (Player-hits (first (World-players aWorld)))
                                                       (cons (STEB-text COORD-STEB1) (Player-misses (first (World-players aWorld)))))
                                          (make-Player (Player-ships (second (World-players aWorld)))
                                                       (Player-unplaced-ships (second (World-players aWorld)))
                                                       (Player-ship-selection (second (World-players aWorld)))
                                                       (cons cpu-attack (Player-hits (second (World-players aWorld))))
                                                       (Player-misses (second (World-players aWorld)))))))]
                                [else
                                 (begin
                                   (place-miss-circle (STEB-text COORD-STEB1))
                                   (make-World 
                                    (World-player-turn aWorld)
                                    (list (make-Player (Player-ships (first (World-players aWorld)))
                                                       (Player-unplaced-ships (first (World-players aWorld)))
                                                       (Player-ship-selection (first (World-players aWorld)))
                                                       (Player-hits (first (World-players aWorld)))
                                                       (cons (STEB-text COORD-STEB1) (Player-misses (first (World-players aWorld)))))
                                          (make-Player (Player-ships (second (World-players aWorld)))
                                                       (Player-unplaced-ships (second (World-players aWorld)))
                                                       (Player-ship-selection (second (World-players aWorld)))
                                                       (Player-hits (second (World-players aWorld)))
                                                       (cons cpu-attack (Player-misses (second (World-players aWorld))))))))])]))]
                  [else aWorld])]
            [else aWorld])]
      [(= 1 (string-length STEBKeyEvent)) (begin (insert-char STEBKeyEvent) aWorld)]
      [else aWorld])))

#|
(check-expect (STEB-cursor (update-steb STEB1 "right")) 8)
(check-expect (STEB-cursor (update-steb STEB2 "left")) 3)
(check-expect (STEB-cursor (update-steb STEB3 "\b")) 0)
(check-expect (STEB-text (update-steb STEB3 "a")) "oaEnter a Coordinate")
(check-expect (STEB-text (update-steb STEB2 "control")) "Dank")
(check-expect (STEB-text (update-steb STEB3 "\r")) "Practice")
|#



;;------------------------[SHIP PLACEMENT KAYHANDLER]---------------



;; determine-columns: nothing -> [listof String]
;; Consumes: nothing
;; Produces: a [listof String] containing all the columns that the ship will take up
(define (determine-columns)
  (local[(define x (+ PLACE-X (image-width PLACE-BOX)))
         (define interval 205)
         (define index 0)
         (define cols empty)
         (define letters (list "A" "B" "C" "D" "E" "F" "G" "H"))
         (define (addletter)
           (cond [(and (> x interval) (<= PLACE-X interval))
                  (set! cols (cons (list-ref letters index) cols))
                  ]
                 [else void]))
         (define (P?)
           (or (>= index (length letters))
               (>= interval 605)))
         (define (BODY)
           (begin
             (addletter)
             (set! interval (+ 50 interval))
             (set! index (add1 index))
             ))
         (define (RESULT)
           cols)]
    (do-until P? BODY RESULT)))


;; determine-rows: nothing -> [listof String]
;; Consumes: nothing
;; Produces: a [listof String] containing all the rows that the ship will take up
(define (determine-rows)
  (local[(define y-span (+ PLACE-Y (image-height PLACE-BOX)))
         (define interval 62)
         (define index 0)
         (define rows empty)
         (define numbers (list "1" "2" "3" "4" "5" "6" "7" "8"))
         (define (addnumber)
           (cond [(and (> y-span interval) (<= PLACE-Y interval))
                  (set! rows (cons (list-ref numbers index) rows))
                  ]
                 [else void]))
         (define (P?)
           (or (>= index (length numbers))
               (>= interval 462)))
         (define (BODY)
           (begin
             (addnumber)
             (set! interval (+ 50 interval))
             (set! index (add1 index))
             ))
         (define (RESULT)
           rows)]
    (do-until P? BODY RESULT)))


;; determine-coordinates: nothing -> [listof Coordinate]
;; Consumes: nothing
;; Produces: a [listof Coordinate] for a new ship
(define (determine-coordinates)
  (local[(define rows (determine-rows))
         (define columns (determine-columns))]
    (cond[(= 1 (length rows))
          (map (lambda (X) (string-append X (first rows))) columns)]
         [else
          (map (lambda (X) (string-append (first columns) X)) rows)])))

(check-expect (determine-coordinates) (list "A3" "A2" "A1"))


;; get-shipimage: aWorld -> image
;; Consumes: a World
;; Produces: an image of the ship that matches the current player's selection
(define (get-shipimage aWorld)
  (cond [(> (image-height PLACE-BOX) (image-width PLACE-BOX))
         (cond[(Patrol? (Player-ship-selection (first (World-players aWorld)))) PATROL-I]
              [(Sub? (Player-ship-selection (first (World-players aWorld)))) SUB-I]
              [(Destroyer? (Player-ship-selection (first (World-players aWorld)))) DESTROYER-I]
              [(Battleship? (Player-ship-selection (first (World-players aWorld)))) BATTLESHIP-I]
              [(Aircraft-Car? (Player-ship-selection (first (World-players aWorld)))) ACC-I])]
        [else
         (cond[(Patrol? (Player-ship-selection (first (World-players aWorld)))) (rotate 90 PATROL-I)]
              [(Sub? (Player-ship-selection (first (World-players aWorld)))) (rotate 90 SUB-I)]
              [(Destroyer? (Player-ship-selection (first (World-players aWorld)))) (rotate 90 DESTROYER-I)]
              [(Battleship? (Player-ship-selection (first (World-players aWorld)))) (rotate 90 BATTLESHIP-I)]
              [(Aircraft-Car? (Player-ship-selection (first (World-players aWorld)))) (rotate 90 ACC-I)])]))


;; place-ship: Player -> [listof Ship]
;; Consumes: Player
;; Produces: [listof Ship] with a ship placed with the proper [listof Coordinates] added
(define (place-ship aPlayer)
  (cond[(Patrol? (Player-ship-selection aPlayer)) 
        (cons (make-Patrol (determine-coordinates)) (Player-ships aPlayer))]
       [(Sub? (Player-ship-selection aPlayer)) 
        (cons (make-Sub (determine-coordinates)) (Player-ships aPlayer))]
       [(Destroyer? (Player-ship-selection aPlayer)) 
        (cons (make-Destroyer (determine-coordinates)) (Player-ships aPlayer))]
       [(Battleship? (Player-ship-selection aPlayer)) 
        (cons (make-Battleship (determine-coordinates)) (Player-ships aPlayer))]
       [(Aircraft-Car? (Player-ship-selection aPlayer)) 
        (cons (make-Aircraft-Car (determine-coordinates)) (Player-ships aPlayer))]))

(check-expect (first (place-ship PLAYER1)) (make-Sub (list "A3" "A2" "A1")))



;; initial (x,y) placement coordinates for the placement box
(define PLACE-X 205)
(define PLACE-Y 62)
;; ship-placement-keyhandler: World Ship-KeyEvent -> World
;; Consumes: a World and KeyEvents related to the placement of ship on to the grid
;; Produces: an updated World
(define (ship-placement-keyhandler aWorld aSKE)
  (local[(define temp 0)
         (define animage ST-SIZE-3)]
    (cond[(key=? aSKE "up") (cond[(>= 62 (- PLACE-Y 50))
                                  (begin (set! PLACE-Y (- 463 (image-height PLACE-BOX))) aWorld)]
                                 [else
                                  (begin (set! PLACE-Y (- PLACE-Y 50)) aWorld)])]
         [(key=? aSKE "down") (cond[(<= 463 (+ 50 (+ PLACE-Y (image-height PLACE-BOX))))
                                    (begin (set! PLACE-Y 62) aWorld)]
                                   [else (begin (set! PLACE-Y (+ PLACE-Y 50)) aWorld)])]
         [(key=? aSKE "right") (cond[(<= 606 (+ PLACE-X 50 (image-width PLACE-BOX)))
                                     (begin (set! PLACE-X 205) aWorld)]
                                    [else (begin (set! PLACE-X (+ PLACE-X 50)) aWorld)])]
         [(key=? aSKE "left") (cond[(>= 205 (- PLACE-X 50))
                                    (begin (set! PLACE-X (- 606 (image-width PLACE-BOX))) aWorld)]
                                   [else (begin (set! PLACE-X (- PLACE-X 50)) aWorld)])]
         [(key=? aSKE "o")
          (begin (set! PLACE-BOX (rectangle (image-height PLACE-BOX) (image-width PLACE-BOX) "solid" "orangered")) aWorld)]
         [(key=? aSKE "p")
          (begin (set! PLACE-BOX ST-SIZE-2) 
                 (make-World (World-player-turn aWorld) 
                             (list (make-Player (Player-ships (first (World-players aWorld))) 
                                                (Player-unplaced-ships (first (World-players aWorld))) 
                                                EX-PATROL1
                                                (Player-hits (first (World-players aWorld))) 
                                                (Player-misses (first (World-players aWorld))))
                                   (second (World-players aWorld)))))]
         [(key=? aSKE "s")
          (begin (set! PLACE-BOX ST-SIZE-3)
                 (make-World (World-player-turn aWorld) 
                             (list (make-Player (Player-ships (first (World-players aWorld))) 
                                                (Player-unplaced-ships (first (World-players aWorld))) 
                                                EX-SUB1
                                                (Player-hits (first (World-players aWorld))) 
                                                (Player-misses (first (World-players aWorld))))
                                   (second (World-players aWorld)))))]
         [(key=? aSKE "d")
          (begin (set! PLACE-BOX ST-SIZE-3)
                 (make-World (World-player-turn aWorld) 
                             (list (make-Player (Player-ships (first (World-players aWorld))) 
                                                (Player-unplaced-ships (first (World-players aWorld))) 
                                                EX-DESTROYER1
                                                (Player-hits (first (World-players aWorld))) 
                                                (Player-misses (first (World-players aWorld))))
                                   (second (World-players aWorld)))))]
         [(key=? aSKE "b")
          (begin (set! PLACE-BOX ST-SIZE-4)
                 (make-World (World-player-turn aWorld) 
                             (list (make-Player (Player-ships (first (World-players aWorld))) 
                                                (Player-unplaced-ships (first (World-players aWorld))) 
                                                EX-BATTLESHIP1
                                                (Player-hits (first (World-players aWorld))) 
                                                (Player-misses (first (World-players aWorld))))
                                   (second (World-players aWorld)))))]
         [(key=? aSKE "a")
          (begin (set! PLACE-BOX ST-SIZE-5)
                 (make-World (World-player-turn aWorld) 
                             (list (make-Player (Player-ships (first (World-players aWorld))) 
                                                (Player-unplaced-ships (first (World-players aWorld))) 
                                                EX-AIRCRAFT-CAR1
                                                (Player-hits (first (World-players aWorld))) 
                                                (Player-misses (first (World-players aWorld))))
                                   (second (World-players aWorld)))))]
         [(key=? aSKE "\r")
          (cond[(and (ships-left? (first (World-players aWorld))) (can-place? (first (World-players aWorld))))
                (begin (set! GAME-GUI 
                             (place-image/align (get-shipimage aWorld) PLACE-X PLACE-Y "left" "top" GAME-GUI)) 
                       (make-World (World-player-turn aWorld) 
                                   (list (make-Player (place-ship (first (World-players aWorld))) 
                                                      (remove-ship (first (World-players aWorld))) 
                                                      (Player-ship-selection (first (World-players aWorld)))
                                                      (Player-hits (first (World-players aWorld))) 
                                                      (Player-misses (first (World-players aWorld))))
                                         (second (World-players aWorld)))))]
               [else aWorld])]
         [else aWorld]))) 

(check-expect (ship-placement-keyhandler GAME-WORLD "up") GAME-WORLD)
(check-expect (ship-placement-keyhandler GAME-WORLD "down") GAME-WORLD)
(check-expect (ship-placement-keyhandler GAME-WORLD "left") GAME-WORLD)
(check-expect (ship-placement-keyhandler GAME-WORLD "right") GAME-WORLD)

#|
(check-expect (ship-placement-keyhandler GAME-WORLD "p") GAME-WORLD)
(check-expect (ship-placement-keyhandler GAME-WORLD "s") GAME-WORLD)
(check-expect (ship-placement-keyhandler GAME-WORLD "d") GAME-WORLD)
(check-expect (ship-placement-keyhandler GAME-WORLD "b") GAME-WORLD)
(check-expect (ship-placement-keyhandler GAME-WORLD "a") GAME-WORLD)
(check-expect (ship-placement-keyhandler GAME-WORLD "o") GAME-WORLD)
(check-expect (ship-placement-keyhandler GAME-WORLD "l") GAME-WORLD)
|#



;;----------------------------[WORLD KEY HANDLER]---------------------------------


;; world-keyhandler: World WorldKeyEvent -> World
;; Consumes: a World and KeyEvents related to the targeting of ships
;; Produces: an updated World
(define (world-keyhandler  aWorld aWKE)
  (cond[(game-begin? (Player-unplaced-ships (first (World-players aWorld))) 
                     (Player-unplaced-ships (second (World-players aWorld))))
        (battle-keyhandler aWorld aWKE)]
       [else (ship-placement-keyhandler aWorld aWKE)]))



;;----------------------------[TICK HANDLER]---------------------------------



;; remove-handler: World -> World
;; Consumes: a World
;; Produces: a world with the dead ships removed from players ship lists
(define (remove-handler aWorld)
  (make-World
   (World-player-turn aWorld)
   (list (make-Player (filter (lambda (X) (not (sunk? X))) (Player-ships (first (World-players aWorld))))
                      (Player-unplaced-ships (first (World-players aWorld)))
                      (Player-ship-selection (first (World-players aWorld)))
                      (Player-hits (first (World-players aWorld)))
                      (Player-misses (first (World-players aWorld))))
         (make-Player (filter (lambda (X) (not (sunk? X))) (Player-ships (second (World-players aWorld))))
                      (Player-unplaced-ships (second (World-players aWorld)))
                      (Player-ship-selection (second (World-players aWorld)))
                      (Player-hits (second (World-players aWorld)))
                      (Player-misses (second (World-players aWorld)))))))



;;------------------------[GUI-FUNCTIONS]--------------------------



;; ship-count: (X -> boolean) [listof Ship] -> number
;; Consumes: a (X -> boolean) f and a [listof Ship]
;; Produces: a number indicating the number of Ships 
;;           of a given type in the list
(define (ship-count f aLoS)
  (foldr (lambda (aship total) (cond[(f aship) (+ 1 total)] 
                                    [else total]))
         0
         aLoS))



;; patrol-count: [listof Ship] -> number
;; Consumes: [listof Ship]
;; Produces: a number indicating the number of patrol ships the list has
(define (patrol-count aLoS) (ship-count Patrol? aLoS))

(check-expect (patrol-count (list EX-PATROL1 EX-SUB1 EX-BATTLESHIP1)) 1)
(check-expect (patrol-count (list EX-PATROL1 EX-PATROL1 EX-BATTLESHIP1)) 2)
(check-expect (patrol-count (list EX-SUB1 EX-BATTLESHIP1)) 0)


;; sub-count:[listof Ship]-> number
;; Consumes:[listof Ship]
;; Produces: a number indicating the number of Subs the list has
(define (sub-count aLoS) (ship-count Sub? aLoS))

(check-expect (sub-count (list EX-PATROL1 EX-SUB1 EX-BATTLESHIP1)) 1)
(check-expect (sub-count (list EX-PATROL1 EX-PATROL1 EX-BATTLESHIP1)) 0)
(check-expect (sub-count (list EX-SUB1 EX-SUB1)) 2)


;; destroyer-count: [listof Ship] -> number
;; Consumes: [listof Ship]
;; Produces: a number indicating the number of destroyer ships the list has
(define (destroyer-count aLoS) (ship-count Destroyer? aLoS))

(check-expect (destroyer-count (list EX-PATROL1 EX-DESTROYER1 EX-DESTROYER1)) 2)
(check-expect (destroyer-count (list EX-PATROL1 EX-PATROL1 EX-BATTLESHIP1)) 0)
(check-expect (destroyer-count (list EX-DESTROYER1 EX-SUB1)) 1)


;; battleship-count: [listof Ship] -> number
;; Consumes: [listof Ship]
;; Produces: a number indicating the number of battleships ships the list has
(define (battleship-count aLoS) (ship-count Battleship? aLoS))

(check-expect (battleship-count (list EX-PATROL1 EX-DESTROYER1 EX-DESTROYER1)) 0)
(check-expect (battleship-count (list EX-PATROL1 EX-PATROL1 EX-BATTLESHIP1)) 1)


;; aircraft-count: [listof Ship] -> number
;; Consumes: [listof Ship]
;; Produces: a number indicating the number of aircraft-carrier ships the list has
(define (aircraft-count aLoS) (ship-count Aircraft-Car? aLoS))

(check-expect (aircraft-count (list EX-PATROL1 EX-DESTROYER1 EX-AIRCRAFT-CAR1)) 1)
(check-expect (aircraft-count (list EX-PATROL1 EX-PATROL1 EX-BATTLESHIP1)) 0)


;; get-hit/miss: [Listof Coordinate] -> image
;; Consumes: a [Listof Coordinate]
;; Produces: The the text of the coordinates placed on top of each other
(define (get-hit/miss aLoC)
  (cond[(empty? aLoC) (text "" 10 "black")]
       [else
        (foldl (lambda (x list) (above list (text x 10 "black"))) (text (first aLoC) 10 "black") (rest aLoC))]))

(check-expect (get-hit/miss (list "D1" "D2" "D3")) (above (text "D1" 10 "black")
                                                          (text "D2" 10 "black")
                                                          (text "D3" 10 "black")))
(check-expect (get-hit/miss empty) (text "" 10 "black"))



;;------------------------[GUI RENDER]--------------------------

(define COUNT-NUMX 120)
(define COUNT-NUMY 370)


;; render-selectionGUI: aWorld -> Image
;; Consumes: a World
;; Produces: an Image of the GUI for player 1
(define (render-selectionGUI aWorld)
  (begin 
    (set! GAME-GUI 
          (place-image/align (get-hit/miss (Player-hits (first (World-players aWorld))))
                             25 125
                             "left" "top"
                             (place-image/align (get-hit/miss (Player-misses (first (World-players aWorld))))
                                                125 125
                                                "left" "top"
                                                GAME-GUI)))
    (place-image/align PLACE-BOX
                       PLACE-X PLACE-Y
                       "left" "top"
                       (place-image (text (number->string (patrol-count (Player-unplaced-ships (first (World-players aWorld))))) 
                                          12 
                                          "black")
                                    COUNT-NUMX 
                                    COUNT-NUMY
                                    (place-image (text (number->string (sub-count (Player-unplaced-ships (first (World-players aWorld))))) 
                                                       12 
                                                       "black")
                                                 COUNT-NUMX 
                                                 (+ COUNT-NUMY 20)
                                                 (place-image (text (number->string (battleship-count (Player-unplaced-ships (first (World-players aWorld))))) 
                                                                    12 
                                                                    "black")
                                                              COUNT-NUMX 
                                                              (+ COUNT-NUMY 40)
                                                              (place-image (text (number->string (destroyer-count (Player-unplaced-ships (first (World-players aWorld))))) 
                                                                                 12 
                                                                                 "black")
                                                                           COUNT-NUMX 
                                                                           (+ COUNT-NUMY 60)
                                                                           (place-image (text (number->string (aircraft-count (Player-unplaced-ships (first (World-players aWorld))))) 
                                                                                              12 
                                                                                              "black")
                                                                                        COUNT-NUMX 
                                                                                        (+ COUNT-NUMY 80)
                                                                                        GAME-GUI))))))))


;; render-battleGUI: aWorld -> Image
;; Consumes: a World
;; Produces: an Image of the GUI for player 1
(define (render-battleGUI aWorld)
  
  (place-image/align (render-steb COORD-STEB1)
                     COORDINATE-BOX-X
                     COORDINATE-BOX-Y
                     "left" "top"
                     (place-image/align (get-hit/miss (Player-hits (first (World-players aWorld))))
                                        25 125
                                        "left" "top"
                                        (place-image/align (get-hit/miss (Player-misses (first (World-players aWorld))))
                                                           125 125
                                                           "left" "top"
                                                           
                                                           (place-image (text (number->string (patrol-count (Player-ships (first (World-players aWorld))))) 
                                                                              12 
                                                                              "black")
                                                                        COUNT-NUMX 
                                                                        COUNT-NUMY
                                                                        (place-image (text (number->string (sub-count (Player-ships (first (World-players aWorld))))) 
                                                                                           12 
                                                                                           "black")
                                                                                     COUNT-NUMX 
                                                                                     (+ COUNT-NUMY 20)
                                                                                     (place-image (text (number->string (battleship-count (Player-ships (first (World-players aWorld))))) 
                                                                                                        12 
                                                                                                        "black")
                                                                                                  COUNT-NUMX 
                                                                                                  (+ COUNT-NUMY 40)
                                                                                                  (place-image (text (number->string (destroyer-count (Player-ships (first (World-players aWorld))))) 
                                                                                                                     12 
                                                                                                                     "black")
                                                                                                               COUNT-NUMX 
                                                                                                               (+ COUNT-NUMY 60)
                                                                                                               (place-image (text (number->string (aircraft-count (Player-ships (first (World-players aWorld))))) 
                                                                                                                                  12 
                                                                                                                                  "black")
                                                                                                                            COUNT-NUMX 
                                                                                                                            (+ COUNT-NUMY 80)
                                                                                                                            GAME-GUI)))))))))

;; render-GUI: aWorld -> Image
;; Consumes: a World
;; Produces: an Image of the GUI for player 1
(define (render-GUI aWorld)
  (cond[(game-begin? (Player-unplaced-ships (first (World-players aWorld))) 
                     (Player-unplaced-ships (second (World-players aWorld))))
        (render-battleGUI aWorld)]
       [else (render-selectionGUI aWorld)]))


(define FINAL-GUI (render-GUI GAME-WORLDF))



(define (test-game aWorld) 
  (big-bang
   aWorld
   (on-draw render-GUI)
   (on-tick remove-handler)
   (on-key world-keyhandler)
   (stop-when game-over? win/lose-screen)
   ))


(test-game GAME-WORLD)

