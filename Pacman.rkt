;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname hw05) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Matthew Martin
; hw05 - Lists
; ITEC380
; https://www.radford.edu/~itec380/2020spring-ibarland/Homeworks/lists/lists.html
(require 2htdp/image)
(require 2htdp/universe)
(require "overlap.rkt")

;Data Def'n a team is:
; a name (non-empty string), AND
; an offense rating (real number), AND
; a defense rating (real number).
(define-struct team (name offense defense))

"john"
"mark"
3.4
6.4
0


;#Default Functions#
;make-team : string real-number real-number
;team-name
;team-offense
;team-defense
;team?

(define (func-for-team a-team)
  (... (team-name a-team)
       ... (team-offense a-team)
       ... (team-defense a-team)))

(define t1 (make-team "Radford Rioters" 3.1 5.5))
(define t2 (make-team "Christiansburg Criminals" 3.0 5.9))
(define t3 (make-team "Blacksburg Bandits" 1.0 0))

;team>> : team team -> boolean
;one team is “greater” than another if its offense is higher than the other’s defense, and its defense is higher than the other’s offense.
(define (team>?  a-team b-team)(and (> (team-offense a-team) (team-defense b-team)) (> (team-defense a-team) (team-offense b-team)) ))

(check-expect (team>? t1 t2) #false)
(check-expect (team>? (make-team "Radford Rioters" 0 0) (make-team "Radford Rioppers" 0 0)) #false)
(check-expect (team>? (make-team "Radford Rioters" 7 7) (make-team "Radford Rioppers" 1 1)) #true)
(check-expect (team>? (make-team "Radford Rioters" 7 1) (make-team "Radford Rioppers" 7 1)) #false)
(check-expect (team>? (make-team "Radford Rioters" 1 7) (make-team "Radford Rioppers" 1 7)) #false)
(check-expect (team>? (make-team "Radford Rioters" 7 7) (make-team "Radford Rioppers" 7 7)) #false)

#|##################################################################################################
###################################### PACMAN GAME #################################################
###########################################################################################parrot6|#
(define GAMESPEED 3)
(define GHOSTSPEED GAMESPEED)
(define PACMANSPEED GAMESPEED)

;Data Def'n a coordinate is:
; a x Coordinate, AND y Coordinate in pixels
(define-struct coordinate (x y))

(define co1 (make-coordinate 100 100))
(define co2 (make-coordinate 0 300))

;Data Def'n a direction is:
; a string "up" "down" "left" "right"

#|####################### PACMAN #########################################################parrot6|#

;Data Def'n a pacman is:
; a coordinate, AND
; and curdir (direction, currently heading)
; an age (natural number)
; and nextdir (direction, to go next)

;(make-pacman coordinate direction natnum direction)
(define-struct pacman (coordinate curdir age nextdir))

(define p1 (make-pacman (make-coordinate 100 100) "right" 0 "right") )
(define p2 (make-pacman co1 "left" 0  "right"))

(define (func-for-pacman a-pacman)
  (... (pacman-coordinate a-pacman)
       ... (pacman-curdir a-pacman)
       ... (pacman-age a-pacman)
       ... (pacman-nextdir a-pacman)
       ))

;glide-pacman: pacman -> pacman
;takes in a pacman, and returns a pacman one "tick" of time later, ignoring any exterior factors like walls or ghosts or wrapping around the screen
(define (glide-pacman a-pacman) (make-pacman
                                 (move (pacman-coordinate a-pacman) (pacman-curdir a-pacman) PACMANSPEED)
                                 (pacman-curdir a-pacman)
                                 (+ 1 (pacman-age a-pacman))
                                 (pacman-nextdir a-pacman)))
(check-expect (glide-pacman p1) (make-pacman (make-coordinate (+ 100 PACMANSPEED) 100) "right" 1 "right"))
(check-expect (glide-pacman p2) (make-pacman (make-coordinate (- 100 PACMANSPEED) 100) "left" 1 "right"))


;draw-pacman : pacman, image -> image,
;overlay pacman on image at pacman's coordinates
(define (draw-pacman pacman background) (draw-all (pacman->image pacman) (pacman-coordinate pacman) background))
(define (pacman->image p) (rotate (direction->angle (pacman-curdir p)) (pacman-graphic (pacman-age p))))

; direction->angle : direction -> real?
; Return an angle to rotate a figure by, based on its direction.
; https://learn.radford.edu/d2l/le/content/158147/viewContent/2455821/View structs-pacman-soln.rkt
(define (direction->angle dir)
  (cond [(string=? dir "left")  180]         
        [(string=? dir "right")   0]         
        [(string=? dir "down")  -90]         
        [(string=? dir "up")    +90]))

;pacman-handle-key : pacman, KeyPress -> pacman
;takes in a pacman and a keypress and handles the event
(define (pacman-handle-key a-pacman KeyPress)
  (make-pacman
   (pacman-coordinate a-pacman)
   (pacman-curdir a-pacman)
   (pacman-age a-pacman)
   (cond [(string=? KeyPress "right") "right"]
         [(string=? KeyPress "left") "left"]
         [(string=? KeyPress "up") "up"]
         [(string=? KeyPress "down") "down"]
         [else (pacman-nextdir a-pacman)])
   )
  )

(check-expect (pacman-handle-key p1 "down")  (make-pacman (make-coordinate 100 100) "right" 0 "down")) 
(check-expect (pacman-handle-key p1 "up")  (make-pacman (make-coordinate 100 100) "right" 0 "up")) 
(check-expect (pacman-handle-key p1 "left")  (make-pacman (make-coordinate 100 100) "right" 0 "left")) 
(check-expect (pacman-handle-key p1 "right")  (make-pacman (make-coordinate 100 100) "right" 0 "right"))

;pacman-collide-ghost? pacman, ghost -> Boolean
;takes in alien and ghost and compares for collision, assuming all object are of 'Tile' size
(define (pacman-collide-ghost? a-pacman a-ghost) (coordinate-collide? (pacman-coordinate a-pacman)(ghost-coordinate a-ghost)))
(check-expect (pacman-collide-ghost? p1 g1) #true)
(check-expect (pacman-collide-ghost?  (glide-pacman p1) g1) #true)
(check-expect (pacman-collide-ghost? p2 (make-ghost (make-coordinate (+ 100 Tile) 100) "left" #false)) #false)
(check-expect (pacman-collide-ghost? p2 (make-ghost (make-coordinate (+ 100 (- Tile 1)) 100) "left" #false)) #true)

;pacman-collide-walls? pacman, list-of-wall -> Boolean
;takes in pacman, list-of-wall and compares for each collision, assuming all object are of 'Tile' size
(define (pacman-collide-ghosts? a-pacman ListOfghost)(cond [(empty? ListOfghost) #false]
                                                           [(cons? ListOfghost) (or (pacman-collide-ghost? a-pacman (first ListOfghost))
                                                                                    (pacman-collide-ghosts? a-pacman (rest ListOfghost)))]))
(check-expect (pacman-collide-ghosts? p1 (cons g1 '())) #true)
(check-expect (pacman-collide-ghosts?  (glide-pacman p1) (cons g1 '())) #true)
(check-expect (pacman-collide-ghosts? p2 (cons (make-ghost (make-coordinate (+ 100 Tile) 100) "left" #false) '())) #false)
(check-expect (pacman-collide-ghosts? p2 (cons (make-ghost (make-coordinate (+ 100 Tile) 100) "left" #false) (cons (make-ghost (make-coordinate (+ 100 (- Tile 1)) 100) "left" #false) '()))) #true)
(check-expect (pacman-collide-ghosts? p2 (cons (make-ghost (make-coordinate (+ 100 (- Tile 1)) 100) "left" #false) '())) #true)

;coordinate-collide? coordinate, coordinate -> Boolean
;takes in two coordinates and compares for collision, assuming all object are of 'Tile' size
(define (coordinate-collide? coord1 coord2) (overlap? (coordinate-x coord1) (coordinate-y coord1) Tile Tile (coordinate-x coord2) (coordinate-y coord2) Tile Tile))
(check-expect (coordinate-collide? co1 co2) #false)
(check-expect (coordinate-collide? co1 co1) #true)

(define (coordinate-collideWallList? coord1 WallList)(if (empty? WallList)
                                                         #false
                                                         (or (coordinate-collide? coord1 (wall-coordinate (first WallList))) (coordinate-collideWallList? coord1 (rest WallList)))))
  
;pacman-collide-wall? pacman, wall -> Boolean
;takes in pacman, wall and compares for collision, assuming all object are of 'Tile' size
(define (pacman-collide-wall? a-pacman wallpiece) (coordinate-collide? (pacman-coordinate a-pacman) (wall-coordinate wallpiece)))
(check-expect (pacman-collide-wall? p1 (make-wall (make-coordinate 100 100))) #true)
(check-expect (pacman-collide-wall? p1 (make-wall (make-coordinate 120 100))) #false)
(check-expect (pacman-collide-wall? p1 (make-wall (make-coordinate 100 120))) #false)

;pacman-collide-walls? pacman, list-of-wall -> Boolean
;takes in pacman, list-of-wall and compares for each collision, assuming all object are of 'Tile' size
(define (pacman-collide-walls? a-pacman ListOfWalls)(cond [(empty? ListOfWalls) #false]
                                                          [(cons? ListOfWalls) (or (pacman-collide-wall? a-pacman (first ListOfWalls))
                                                                                   (pacman-collide-walls? a-pacman (rest ListOfWalls)))]))

(check-expect (pacman-collide-walls? p1 (cons (make-wall (make-coordinate 100 100)) (cons w1 empty))) #true)
(check-expect (pacman-collide-walls? p1 (cons (make-wall (make-coordinate 100 100)) empty)) #true)
(check-expect (pacman-collide-walls? p1 (cons (make-wall (make-coordinate 120 120)) empty)) #false)

(define (pacman-collide-dot? a-pacman dot) (coordinate-collide? (pacman-coordinate a-pacman) (dot-coordinate dot)))
(check-expect (pacman-collide-dot? p1 (make-dot (make-coordinate 100 100))) #true)
(check-expect (pacman-collide-dot? p1 (make-dot (make-coordinate 120 100))) #false)
(check-expect (pacman-collide-dot? p1 (make-dot (make-coordinate 100 120))) #false)

;move-pacman : pacman list-of-walls -> pacman
;tries to move pacman based on curdir and nextdir
(define (move-pacman apc ListOfWalls) (move-pacmanAger apc ListOfWalls 0))
(define (move-pacmanAger apc ListOfWalls age)
  (cond
    [(> age 5) (error "pacman inside a wall")]
    [(! (pacman-collide-walls? (glide-pacman (make-pacman (pacman-coordinate apc)(pacman-nextdir apc)(pacman-age apc)(pacman-nextdir apc))) ListOfWalls)) (glide-pacman (make-pacman (pacman-coordinate apc)(pacman-nextdir apc)(pacman-age apc)(pacman-nextdir apc)))]
    [(! (pacman-collide-walls? (glide-pacman apc) ListOfWalls)) (glide-pacman apc)]
    [else (move-pacmanAger (make-pacman (pacman-coordinate apc)(nextmove (pacman-curdir apc))(pacman-age apc)(nextmove (pacman-curdir apc))) ListOfWalls (add1 age))]))

(check-expect (move-pacman (make-pacman (make-coordinate 100 100) "right" 0 "right") '()) (glide-pacman p1))
(check-expect (move-pacman (make-pacman (make-coordinate 100 100) "up" 0 "right") '()) (glide-pacman p1))
(check-expect (move-pacman (make-pacman (make-coordinate 100 100) "right" 0 "up") (cons (make-wall (make-coordinate 120 100)) '())) (make-pacman (make-coordinate 100 97) "up" 1 "up"))
(check-expect (move-pacman (make-pacman (make-coordinate 100 100) "right" 0 "right") (cons (make-wall (make-coordinate 120 100)) '())) (make-pacman (make-coordinate 100 97) "up" 1 "up"))
(check-expect (move-pacman (make-pacman (make-coordinate 80 100) "right" 0 "right") tallwall) (make-pacman (make-coordinate 80 97) "up" 1 "up"))
;(check-expect (move-pacman (make-pacman (make-coordinate 100 100) "right" 0 "right") tallwall) (make-pacman (make-coordinate 80 97) "up" 1 "up")) ;pacman inside wall
 
#|####################### GHOST ##########################################################parrot6|#

;Data Def'n a ghost is:
; a coordinate, AND
; an direction,
; and vulnerable? (boolean)

(define-struct ghost (coordinate curdir vulnerable?))
;(make-ghost coordinate curdir Boolean)

(define g1 (make-ghost (make-coordinate 100 100) "right" #false))
(define g2 (make-ghost co1 "left" #true))


(define (func-for-ghost a-ghost)
  (... (ghost-coordinate a-ghost)
       ... (ghost-curdir a-ghost)
       ... (ghost-vulnerable? a-ghost)))

;Data Def'n a list-of-ghost is:
; a ghost , AND
; a list
;ghost : ghost?
;list : list?
(cons g1 (cons g2 empty))
(define g1234 (cons (make-ghost (make-coordinate 100 100) "right" #false) (cons (make-ghost (make-coordinate 200 200) "right" #false) (cons (make-ghost (make-coordinate 300 300) "right" #false) (cons (make-ghost (make-coordinate 400 400) "right" #false) empty)))))
empty
'()

;template func-forlistofghosts
(define (func-forlistofghosts ghostList) (... [(empty? ghostList) '()]
                                              [(cons? ghostList) (cons (make-ghost (ghost-coordinate (first ghostList))
                                                                                   (ghost-curdir (first ghostList))
                                                                                   (ghost-vulnerable? (first ghostList))) (func-forlistofghosts (cdr ghostList)))]))
  
;glide-ghost: ghost -> ghost
;takes in a ghost, and returns a ghost one "tick" of time later, ignoring any exterior factors like walls or pacmans or wrapping around the screen
(define (glide-ghost a-ghost) (make-ghost
                               (move (ghost-coordinate a-ghost) (ghost-curdir a-ghost) GHOSTSPEED)
                               (ghost-curdir a-ghost)
                               (ghost-vulnerable? a-ghost)))

(check-expect (glide-ghost g1) (make-ghost (make-coordinate (+ 100 GAMESPEED) 100) "right" #false))
(check-expect (glide-ghost g2) (make-ghost (make-coordinate (- 100 GAMESPEED) 100) "left" #true))

;draw-ghost : ghost, image -> image,
;overlay ghost on image at ghost's coordinates
(define (draw-ghost ghost background) (draw-all ghost-graphic (ghost-coordinate ghost) background))
(check-expect (draw-ghost g1 GameBoard) (place-image ghost-graphic 100 100 GameBoard))

;ghost-collide-|wall|? ghost, wall -> Boolean
;takes in ghost, wall and compares for collision, assuming all object are of 'Tile' size
(define (ghost-collide-|wall|? a-ghost wallpiece) (coordinate-collide? (ghost-coordinate a-ghost) (wall-coordinate wallpiece)))
(check-expect (ghost-collide-wall? g1 (make-wall (make-coordinate 100 100))) #true)
(check-expect (ghost-collide-wall? g1 (make-wall (make-coordinate 120 100))) #false)
(check-expect (ghost-collide-wall? g1 (make-wall (make-coordinate 100 120))) #false)

;ghost-collide-|walls|? ghost, list-of-wall -> Boolean
;takes in ghost, list-of-wall and compares for each collision, assuming all object are of 'Tile' size
(define (ghost-collide-|walls|? a-ghost ListOfWalls)(cond [(empty? ListOfWalls) #false]
                                                          [(cons? ListOfWalls) (or (ghost-collide-|wall|? a-ghost (first ListOfWalls))
                                                                                   (ghost-collide-|walls|? a-ghost (rest ListOfWalls)))]
                                                          [else "not a list"]))

(check-expect (ghost-collide-|walls|? g1 (cons (make-wall (make-coordinate 100 100)) (cons w1 empty))) #true)
(check-expect (ghost-collide-|walls|? g1 (cons (make-wall (make-coordinate 100 100)) empty)) #true)
(check-expect (ghost-collide-|walls|? g1 (cons (make-wall (make-coordinate 120 120)) empty)) #false)

;draw-ghosts : list-of-ghost, image → image
;draws a ghost at its coordinates on given image
(define (draw-ghosts ghostList bg) (cond [(empty? ghostList) bg]
                                         [(cons? ghostList) (draw-ghost (first ghostList) (draw-ghosts (cdr ghostList) bg))]))

(check-expect (draw-ghosts (cons g1 '()) GameBoard) (place-image ghost-graphic 100 100 GameBoard))

(define (ghost-move? a-ghost ListOfWall direction)(cond
                                                    [(string=? direction (first moves))(! (ghost-collide-|walls|? (glide-ghost (make-ghost (ghost-coordinate a-ghost) (first moves) (ghost-vulnerable? a-ghost)) ) ListOfWall))]
                                                    [(string=? direction (second moves))(! (ghost-collide-|walls|? (glide-ghost (make-ghost (ghost-coordinate a-ghost) (second moves) (ghost-vulnerable? a-ghost)) ) ListOfWall))]
                                                    [(string=? direction (third moves))(! (ghost-collide-|walls|? (glide-ghost (make-ghost (ghost-coordinate a-ghost) (third moves) (ghost-vulnerable? a-ghost)) ) ListOfWall))]
                                                    [(string=? direction (fourth moves))(! (ghost-collide-|walls|? (glide-ghost (make-ghost (ghost-coordinate a-ghost) (fourth moves) (ghost-vulnerable? a-ghost)) ) ListOfWall))]
                                                    [else "wrong direction string?"]))

(check-expect (ghost-move? g1 (cons (make-wall (make-coordinate 120 100)) '()) "right") #false)
(check-expect (ghost-move? g1 (cons (make-wall (make-coordinate 100 80)) '()) "up") #false)
(check-expect (ghost-move? g1 (cons (make-wall (make-coordinate 100 120)) '()) "down") #false)
(check-expect (ghost-move? g1 (cons (make-wall (make-coordinate 80 100)) '()) "left") #false)
(check-expect (ghost-move? g1 '() "left") #true)
(check-expect (ghost-move? g1 '() "right") #true)
(check-expect (ghost-move? g1 '() "up") #true)
(check-expect (ghost-move? g1 '() "down") #true)

(define (ghost-move a-ghost direction)(cond
                                        [(string=? direction (first moves))(glide-ghost (make-ghost (ghost-coordinate a-ghost) (first moves) (ghost-vulnerable? a-ghost)) )]
                                        [(string=? direction (second moves))(glide-ghost (make-ghost (ghost-coordinate a-ghost) (second moves) (ghost-vulnerable? a-ghost)) )]
                                        [(string=? direction (third moves))(glide-ghost (make-ghost (ghost-coordinate a-ghost) (third moves) (ghost-vulnerable? a-ghost)) )]
                                        [(string=? direction (fourth moves))(glide-ghost (make-ghost (ghost-coordinate a-ghost) (fourth moves) (ghost-vulnerable? a-ghost)) )]
                                        [else "wrong direction string"]))


;move-ghost : ghost, list-of-wall → ghost
(define (move-ghost a-ghost ListOfWall) (move-ghostAger a-ghost ListOfWall 0))

(define (move-ghostAger a-ghost ListOfWall age)
  (if (> age 5)
      (error "ghost inside a wall")
      (if (ghost-move? a-ghost ListOfWall (ghost-curdir a-ghost))
          (ghost-move a-ghost (ghost-curdir a-ghost))
          (move-ghostAger (make-ghost (ghost-coordinate a-ghost) (nextmove (ghost-curdir a-ghost)) (ghost-vulnerable? a-ghost)) ListOfWall (add1 age)))))

(check-expect (move-ghost g1 '()) (glide-ghost g1))
(check-expect (move-ghost (make-ghost (make-coordinate 80 100) "right" #false) (cons (make-wall (make-coordinate 100 100)) '())) (make-ghost (make-coordinate 80 (- 100 GAMESPEED)) "up" #false))
(check-expect (move-ghost (make-ghost (make-coordinate 80 120) "left" #false) corner100100wall) (glide-ghost (make-ghost (make-coordinate 80 120) "left" #false)))
(check-expect (move-ghost (make-ghost (make-coordinate 80 120) "right" #false) corner100100wall) (glide-ghost (make-ghost (make-coordinate 80 120) "down" #false)))
(check-expect (move-ghost (make-ghost (make-coordinate 80 120) "down" #false) corner100100wall) (glide-ghost (make-ghost (make-coordinate 80 120) "down" #false)))
(check-expect (move-ghost (make-ghost (make-coordinate 80 80) "down" #false) corner100100wall) (glide-ghost (make-ghost (make-coordinate 80 80) "left" #false)))

;move-ghost* : list-of-ghost, list-of-wall → list-of-ghost
(define (move-ghost* ghostList ListOfWall)(if (empty? ghostList)
                                              '()
                                              (cons (move-ghost (first ghostList) ListOfWall) (move-ghost* (rest ghostList) ListOfWall)))) ;add ghost AI here?
                                              
(check-expect (move-ghost* (cons g1 '()) '()) (cons (glide-ghost g1) '()))
(check-expect (move-ghost* (cons g1 (cons g2 '())) '()) (cons (glide-ghost g1) (cons (glide-ghost g2) '())))



(define moves (cons "up" (cons "down" (cons "left" (cons "right" '())))))
(define (nextmove direction)(indexat (max 1 (modulo (+ 1 (indexof direction moves)) 5)) moves))
(check-expect (nextmove "up") "down")
(check-expect (nextmove "down") "left")
(check-expect (nextmove "left") "right")
(check-expect (nextmove "right") "up")


(define (lengthof stringlist)(if (empty? stringlist) 0 (+ 1 (lengthof (rest stringlist)))))
(check-expect (lengthof moves) 4)
(check-expect (lengthof (cons "1" (cons "1" (cons "1" (cons "1" '()))))) 4)
(check-expect (lengthof (cons "1" (cons "1" '()))) 2)
(check-expect (lengthof '()) 0)

;indexof: string list-of-string -> number
;returns index of a item in a list starting at 1
(define (indexof stringitem stringlist) (cond [(empty? stringlist) (error stringitem)]
                                              [(cons? stringlist) (if (string=? stringitem (first stringlist)) 1 (+ 1 (indexof stringitem (rest stringlist))))]))

(check-expect (indexof "up" moves) 1)
(check-expect (indexof "down" moves) 2)
(check-expect (indexof "right" moves) 4)
(check-expect (indexof "left" moves) 3)

(define (indexat num stringlist)(cond [(= 1 num)  (first stringlist)]
                                      [(cons? stringlist) (indexat (- num 1) (rest stringlist))]
                                      [else "not a list"]))
(check-expect (indexat 3 moves) "left")
(check-expect (indexat 1 moves) "up")
(check-expect (indexat 2 moves) "down")
(check-expect (indexat 4 moves) "right")

(define (! bool)(not bool))


#|####################### GRAPHICS ##########################################################parrot6|#


;Tile# : number -> number
;gives equiv cordinate for tile number for easy logic
(define (Tile# num)(+ (/ Tile 2) (* Tile (- num 1))))
(check-expect (- (Tile# 2)(Tile# 1)) Tile)
(check-expect (Tile# 1) (/ Tile 2)) 
(check-expect (Tile# 3) (+ (/ Tile 2) (* 2 Tile ) ))

;Tile#? : number -> number
;gives equiv tile for coord
(define (Tile#? coord) (if (integer? (/ (- coord (/ Tile 2)) Tile))
                           (add1 (/ (- coord (/ Tile 2)) Tile))
                           (error "coordinate doesnt land on a 'Tile'")))
(check-expect (Tile#? (Tile# 1)) 1)
(check-expect (Tile#? (Tile# 39)) 39)


(define Tile 20); 20px Scale everything by this later if needed
(define (pacman-graphic age) (place-image
                              (scale/xy (+ 1 (* 5  (sawtooth age 5))) 1 (rotate -135 (crop 0 0 (/ Tile 2) (/ Tile 2) (circle (/ Tile 2) "solid" "black" ))))
                              (+ (/ Tile 2) (/ (* (/ Tile 2) (+ 1 (* 5  (sawtooth age 5)))) 2) )
                              (/ Tile 2)
                              (circle (/ Tile 2) "solid" "yellow" )))

(define (sawtooth x period/2)
  (/ (abs (- (modulo (- x period/2) (* 2 period/2)) period/2))
     period/2))

(define wall-graphic (overlay (square (* Tile .95) "solid" "black")(square Tile "solid" "white"))) 
(define dot-graphic (overlay (circle (/ (* (/ Tile 2) .9) 2) "solid" "yellow")(circle (/ (/ Tile 2) 2) "solid" "white")))
(define ghost-graphic (radial-star (/ Tile 2) (/ Tile 4) (/ Tile 2) "outline" "lime"))




#|####################### WORLD #############################################################parrot6|#
(define NumGameTiles 21)
(define GameBoardHeight (* Tile NumGameTiles))
(define GameBoardWidth (* Tile NumGameTiles))
(define GameBoard (rectangle GameBoardHeight GameBoardWidth "solid" "black"))
(define GameBoardRegion (rectangle GameBoardHeight GameBoardWidth 0 "black"))

;Data Def'n a wall is:
; a coordinate
(define-struct wall (coordinate))
(define w1 (make-wall (make-coordinate 80 80)))
(define w2 (make-wall (make-coordinate 180 180)))

;draw-wall : wall, image -> image,
;overlay wall on image at wall's coordinates
(define (draw-wall wall background) (draw-all wall-graphic (wall-coordinate wall) background))
(define (draw-walls listofwall background)(cond [(empty? listofwall) background]
                                                [(cons? listofwall) (draw-all wall-graphic (wall-coordinate (first listofwall)) (draw-walls (rest listofwall) background))]))

(check-expect (draw-wall w1 GameBoard) (place-image wall-graphic 80 80 GameBoard))
(check-expect (draw-walls (cons w1 (cons w2 '())) GameBoard) (place-image wall-graphic 80 80 (place-image wall-graphic 180 180 GameBoard)))

;consAppend: list list -> list
;combines the items to items2, without retaining ordering (for walls)
(define (consAppend items items2)(cond [(empty? items) items2]
                                       [(empty? items2) items]
                                       [(cons? items2) (consAppend (rest items) (cons (first items) items2))]))
(check-expect (consAppend (cons "a" '()) (cons "b" '())) (cons "a" (cons "b" '())))
(check-expect (consAppend (cons "a" (cons "b" (cons "b" '()))) (cons "b" '())) (cons "b" (cons "b" (cons "a" (cons "b" '())))))
(check-expect (consAppend (cons "a" '()) '()) (cons "a" '()))

;make-wall-horiz coordinate natural-number -> list of walls
;takes in a coordinate and places 'length' number of walls right
(define (make-wall-horiz coord length) (cond [(= 0 length) '()]
                                             [(coordinate? coord) (cons (make-wall coord) (make-wall-horiz (make-coordinate (+ Tile (coordinate-x coord))(coordinate-y coord)) (sub1 length)))]
                                             ))
(check-expect (make-wall-horiz (make-coordinate 100 100) 1) (cons (make-wall (make-coordinate 100 100)) '()) )
(check-expect (make-wall-horiz (make-coordinate 100 100) 2) (cons (make-wall (make-coordinate 100 100)) (cons (make-wall (make-coordinate 120 100)) '())) )

;make-wall-vert coordinate natural-number -> list of walls
;takes in a coordinate and places 'length' number of walls to the right
(define (make-wall-vert coord length) (cond [(= 0 length) '()]
                                            [(coordinate? coord) (cons (make-wall coord) (make-wall-vert (make-coordinate (coordinate-x coord)(- (coordinate-y coord) Tile)) (sub1 length)))]
                                            ))

(check-expect (make-wall-vert (make-coordinate 100 100) 1) (cons (make-wall (make-coordinate 100 100)) '()) )
(check-expect (make-wall-vert (make-coordinate 100 100) 2) (cons (make-wall (make-coordinate 100 100)) (cons (make-wall (make-coordinate 100 80)) '())) )


;Data Def'n a list-of-wall is:
; a wall , AND
; a list
;dot : dot?
;list : cons?

(define (reflectXwall wall)(make-wall (make-coordinate (Tile# (add1 (- NumGameTiles (Tile#? (coordinate-x (wall-coordinate wall))))))
                                                       (coordinate-y (wall-coordinate wall))
                                                       )))
(check-expect (reflectXwall (make-wall (make-coordinate (Tile# 1) (Tile# 1)))) (make-wall (make-coordinate (Tile# 41) (Tile# 1))))
(check-expect (reflectXwall (make-wall (make-coordinate (Tile# 39) (Tile# 39)))) (make-wall (make-coordinate (Tile# 3) (Tile# 39))))
(check-expect (reflectYwall (make-wall (make-coordinate (Tile# 1) (Tile# 1)))) (make-wall (make-coordinate (Tile# 1) (Tile# 41))))
(check-expect (reflectYwall (make-wall (make-coordinate (Tile# 39) (Tile# 39)))) (make-wall (make-coordinate (Tile# 39) (Tile# 3))))

(define (shiftXwall wall tiles)(make-wall (make-coordinate (+ (coordinate-x (wall-coordinate wall)) (* Tile tiles))
                                                           (coordinate-y (wall-coordinate wall))
                                                           )))
(check-expect (shiftXwall (make-wall (make-coordinate (Tile# 1) (Tile# 1))) 1) (make-wall (make-coordinate(Tile# 2)  (Tile# 1))))
(check-expect (shiftXwall (make-wall (make-coordinate (Tile# 11) (Tile# 11))) -1) (make-wall (make-coordinate (Tile# 10) (Tile# 11) )))

(define (shiftXwalls walls tiles)(if (empty? walls)
                                     '()
                                     (cons (shiftXwall (first walls) tiles) (shiftXwalls (rest walls) tiles))
                                     ))
(check-expect (shiftXwalls (cons (make-wall (make-coordinate (Tile# 1) (Tile# 1))) '()) 1) (cons (make-wall (make-coordinate (Tile# 2) (Tile# 1) )) '()))

(define (reflectXwalls walls)(if (empty? walls)
                                 '()
                                 (cons (reflectXwall (first walls)) (reflectXwalls (rest walls)))
                                 ))

(define (reflectYwall wall)(make-wall (make-coordinate (coordinate-x (wall-coordinate wall))
                                                       (Tile# (add1 (- NumGameTiles (Tile#? (coordinate-y (wall-coordinate wall))))))
                                                       )))
(check-expect (reflectYwall (make-wall (make-coordinate (Tile# 1) (Tile# 1)))) (make-wall (make-coordinate (Tile# 1) (Tile# 41))))
(check-expect (reflectYwall (make-wall (make-coordinate (Tile# 39) (Tile# 39)))) (make-wall (make-coordinate (Tile# 39) (Tile# 3))))

(define (shiftYwall wall tiles)(make-wall (make-coordinate (coordinate-x (wall-coordinate wall))
                                                           (+ (coordinate-y (wall-coordinate wall)) (* Tile tiles))
                                                           )))
(check-expect (shiftYwall (make-wall (make-coordinate (Tile# 1) (Tile# 1))) 1) (make-wall (make-coordinate (Tile# 1) (Tile# 2))))
(check-expect (shiftYwall (make-wall (make-coordinate (Tile# 11) (Tile# 11))) -1) (make-wall (make-coordinate (Tile# 11) (Tile# 10))))

(define (shiftYwalls walls tiles)(if (empty? walls)
                                     '()
                                     (cons (shiftYwall (first walls) tiles) (shiftYwalls (rest walls) tiles))
                                     ))
(check-expect (shiftYwalls (cons (make-wall (make-coordinate (Tile# 1) (Tile# 1))) '()) 1) (cons (make-wall (make-coordinate (Tile# 1) (Tile# 2))) '()))


(define (reflectYwalls walls)(if (empty? walls)
                                 '()
                                 (cons (reflectYwall (first walls)) (reflectYwalls (rest walls)))
                                 ))

(define tallwall (cons (make-wall (make-coordinate 100 100)) (cons (make-wall (make-coordinate 100 120)) (cons (make-wall (make-coordinate 100 140)) (cons (make-wall (make-coordinate 100 160)) '())))))
(define corner100100wall (cons (make-wall (make-coordinate 100 100)) (cons (make-wall (make-coordinate 100 120)) (cons (make-wall (make-coordinate 100 140)) (cons (make-wall (make-coordinate 100 160)) (cons (make-wall (make-coordinate 80 100)) (cons (make-wall (make-coordinate 60 100)) '())))))))
(define 8080deadend (cons (make-wall (make-coordinate 100 100))
                          (cons (make-wall (make-coordinate 100 120))
                                (cons (make-wall (make-coordinate 100 140))
                                      (cons (make-wall (make-coordinate 100 160))
                                            (cons (make-wall (make-coordinate 80 100))
                                                  (cons (make-wall (make-coordinate 60 100)) (cons (make-wall (make-coordinate 80 140)) (cons (make-wall (make-coordinate 60 140)) '())))))))))

(define topborder (make-wall-horiz (make-coordinate (Tile# 1)(Tile# 1)) NumGameTiles))
(define leftborder (make-wall-vert (make-coordinate (Tile# 1)(Tile# (- NumGameTiles 1))) NumGameTiles))

(define GameBoardBorder (consAppend leftborder (consAppend (reflectXwalls leftborder)(consAppend topborder (consAppend topborder (reflectYwalls topborder))))))
(define topleftcorner (consAppend (make-wall-horiz (make-coordinate  (Tile# 3) (Tile# 3)) (round (/ NumGameTiles 8))) (make-wall-vert (make-coordinate  (Tile# 3) (Tile# (+ 3 (sub1 (round (/ NumGameTiles 8)))))) (sub1 (round (/ NumGameTiles 8))))))
(define bottomleftcorner (reflectYwalls topleftcorner))
(define toprightcorner (reflectXwalls topleftcorner))
(define bottomrightcorner (reflectYwalls toprightcorner))
(define topmidwall (make-wall-horiz (make-coordinate (Tile# (round (/ NumGameTiles 4))) (Tile# 3)) (round (/ NumGameTiles 4))))
(define botmidwall (reflectYwalls (reflectXwalls topmidwall)))
(define topvertwall (make-wall-vert (make-coordinate (Tile# (round (* NumGameTiles (/ 2 3)))) (Tile# (+ 1 (round (/ NumGameTiles 3))))) (round (/ NumGameTiles 3))))
(define botvertwall (reflectYwalls (reflectXwalls topvertwall)))
(define midhorzwall (make-wall-horiz (make-coordinate (Tile# (round (* NumGameTiles (/ 1 2)))) (Tile# (+ 2 (round (/ NumGameTiles 3))))) (round (/ NumGameTiles 3))))
(define midhorzwallInv (reflectYwalls (reflectXwalls midhorzwall)))
(define midvertwall (make-wall-vert (make-coordinate (Tile# (sub1 (round (* NumGameTiles (/ 1 2))))) (Tile# (+ 2 (round (/ NumGameTiles 2))))) (round (/ NumGameTiles 3))))
(define midvertwallInv (reflectYwalls (reflectXwalls midvertwall)))

(define combinedboardwalls (consAppend
                            midvertwallInv
                            (consAppend
                            midvertwall
                            (consAppend
                            midhorzwall
                            (consAppend
                            midhorzwallInv
                            (consAppend
                            topvertwall
                            (consAppend
                            botvertwall
                            (consAppend
                            topmidwall
                            (consAppend
                            botmidwall
                            (consAppend
                            bottomrightcorner
                            (consAppend
                             bottomleftcorner
                             (consAppend
                              toprightcorner
                              (consAppend
                               topleftcorner
                               GameBoardBorder)
                              ))))))))))))
;Data Def'n a dot is:
; a coordinate
(define-struct dot (coordinate))
(define d1 (make-dot (make-coordinate 100 100)))
(define d2 (make-dot (make-coordinate 80 80)))

;Data Def'n a list-of-dot is:
; a dot , AND
; a list
;dot : dot?
;list : list?

(cons d1 (cons d2 empty))
(define d123 (cons (make-dot (make-coordinate 100 100)) (cons (make-dot (make-coordinate 200 200)) (cons (make-dot (make-coordinate 300 300)) '()))))
empty
'()


;spamdots -> list-of-walls number number number -> list-of-dots
;chance to spawn a dot on any 'Tile' unoccupied by a wall up to maxdots (1/50 chance up to 1/3, 1/2, 1/1) chance as maxdots decrements, will stop when reaching tile 40 40
(define (spamdots listofwalls maxdots col row)(if (= 0 maxdots)
                                                  '()
                                                  (if (coordinate-collideWallList? (make-coordinate (Tile# col) (Tile# row)) listofwalls)
                                                      (spamdots listofwalls maxdots (add1 col) row)
                                                      (if (>= col NumGameTiles)
                                                          (if (>= row NumGameTiles)
                                                              '()
                                                              (spamdots listofwalls maxdots 1 (add1 row)))
                                                          (if (= 1 (random (min 50 maxdots)))
                                                              (cons (make-dot (make-coordinate (Tile# col) (Tile# row))) (spamdots listofwalls (sub1 maxdots) (add1 col) row))
                                                              (spamdots listofwalls  maxdots (add1 col) row))
                                                          
                                                          ))))


  
;draw-dot : dot, image -> image,
;overlay dot on image at dot's coordinates
(define (draw-dot dot background) (draw-all dot-graphic (dot-coordinate dot) background))
  
(check-expect (draw-dot d1 GameBoard) (place-image dot-graphic 100 100 GameBoard))
(check-expect (draw-dot d2 GameBoard) (place-image dot-graphic 80 80 GameBoard))
(check-expect (draw-dot (make-dot (make-coordinate 0 0)) GameBoard) (place-image dot-graphic 0 0 GameBoard))
(check-expect (draw-dot (make-dot (make-coordinate GameBoardWidth 0)) GameBoard) (place-image dot-graphic GameBoardWidth 0 GameBoard))
(check-expect (draw-dot (make-dot (make-coordinate GameBoardWidth GameBoardHeight)) GameBoard) (place-image dot-graphic GameBoardWidth GameBoardHeight GameBoard))

;draw-dot : list-of-dot, image -> image,
;overlay dot on image at dot's coordinates
(define (draw-dots list-of-dot background) (if (empty? list-of-dot)
                                               background
                                               (draw-all dot-graphic (dot-coordinate (first list-of-dot)) (draw-dots (rest list-of-dot) background))))

(check-expect (draw-dots (cons d2 '()) GameBoard) (place-image dot-graphic 80 80 GameBoard))


;dots-remaining : pacman, List-of-dot → List-of-dot
;returns all dots not colliding with the pacman
(define (dots-remaining a-pacman ListOfDots)(cond [(empty? ListOfDots) '()]
                                                  [(cons? ListOfDots)(if (pacman-collide-dot? a-pacman (first ListOfDots))
                                                                         (dots-remaining a-pacman (rest ListOfDots))
                                                                         (cons (first ListOfDots) (dots-remaining a-pacman (rest ListOfDots))))]
                                                  [else "wrong format"]))

(check-expect (dots-remaining p1 d123) (cons (make-dot (make-coordinate 200 200)) (cons (make-dot (make-coordinate 300 300)) '())))
(check-expect (dots-remaining p1 '()) '())
(check-expect (dots-remaining p1 (cons d1 '())) '())

;draw-all : image, coordinate, image -> image,
;places image1 on image2 at coordinate (place-image that takes a 'coordinate')
(define (draw-all graphic coord background) (place-image graphic (coordinate-x coord) (coordinate-y coord) background))
(check-expect (draw-all dot-graphic (make-coordinate 0 0) GameBoard) (place-image dot-graphic 0 0 GameBoard))


  
;move: coordinate direction -> coordinate
;increments coordinates by corresponding direction and GAMESPEED
(define (move coord direction speed)(cond
                                      [(string=? direction "up")(make-coordinate (coordinate-x coord) (- (coordinate-y coord) speed))]
                                      [(string=? direction "down")(make-coordinate (coordinate-x coord) (+ (coordinate-y coord) speed))]
                                      [(string=? direction "left")(make-coordinate (- (coordinate-x coord) speed) (coordinate-y coord))]
                                      [(string=? direction "right")(make-coordinate (+ (coordinate-x coord) speed) (coordinate-y coord))]
                                      [else coord]))

(check-expect (move (make-coordinate 100 100) "right" GAMESPEED) (make-coordinate (+ GAMESPEED 100) 100))
(check-expect (move (make-coordinate 100 100) "left" GAMESPEED) (make-coordinate (- 100 GAMESPEED) 100))
(check-expect (move (make-coordinate 100 100) "up" GAMESPEED) (make-coordinate  100 (- 100 GAMESPEED)))
(check-expect (move (make-coordinate 100 100) "down" GAMESPEED) (make-coordinate  100 (+ GAMESPEED 100)))

;Data Def'n a world is:
; one pacman, AND
; a list-of-dot, AND
; a list-of-ghost. AND
; a list-of-wall
(define-struct world (APM LOD LOG LOW))


  
(make-world p1 d123 g1234 tallwall)
(define world2 (make-world p2 d123 g1234 tallwall))
(define world1 (make-world
                (make-pacman (make-coordinate (Tile# 2) (Tile# 2)) "right" 0 "right")
                (spamdots combinedboardwalls 50 1 1)
                (cons (make-ghost (make-coordinate (Tile# 2) (Tile# 2)) "down" #false) (cons (make-ghost (make-coordinate (Tile# 2) (Tile# 2)) "right" #false) '()))
                combinedboardwalls))
(make-world p2 '() '() '())

  
(define (worldfuncs a-world)(...
                             (world-APM a-world)
                             (world-LOD a-world)
                             (world-LOG a-world)
                             (world-LOW a-world)))

;update-world : world → world
;takes in a world and returns a world one 'tick' later
(define (update-world a-world)(make-world (move-pacman (world-APM a-world) (world-LOW a-world))
                                          (dots-remaining (move-pacman (world-APM a-world) (world-LOW a-world)) (world-LOD a-world))
                                          (move-ghost* (world-LOG a-world) (world-LOW a-world))
                                          (world-LOW a-world)))
(check-expect (update-world (make-world p1 '() g1234 '())) (make-world (move-pacman p1 '()) '() (move-ghost* g1234 '()) '()))
(check-expect (update-world (make-world p1 d123 g1234 '())) (make-world (move-pacman p1 '())  (cons  (make-dot (make-coordinate 200 200))  (cons   (make-dot (make-coordinate 300 300))   '())) (move-ghost* g1234 '()) '()))

;world-handle-key : world, keypress → world
;takes in a world and a keypress and processes the keypress action
(define (world-handle-key a-world KeyPress)(make-world
                                            (pacman-handle-key (world-APM a-world) KeyPress)
                                            (world-LOD a-world)
                                            (world-LOG a-world)
                                            (world-LOW a-world)))

(check-expect (world-handle-key (make-world p1 '() '() '()) "down") (make-world (make-pacman (make-coordinate 100 100) "right" 0 "down") '() '() '()))
(check-expect (world-handle-key (make-world p1 '() '() '()) "up")  (make-world (make-pacman (make-coordinate 100 100) "right" 0 "up") '() '() '())) 
(check-expect (world-handle-key (make-world p1 '() g2 '()) "up")  (make-world (make-pacman (make-coordinate 100 100) "right" 0 "up") '() g2 '())) 


;draw-world : world -> image
;takes in a world and draws each struct piece ontop of the 'GameBoard'
(define (draw-world a-world)(draw-pacman (world-APM a-world) (draw-ghosts (world-LOG a-world) (draw-walls (world-LOW a-world) (draw-dots (world-LOD a-world) GameBoard)))))

(check-expect (draw-world world2) (draw-pacman p2 (draw-ghosts g1234 (draw-walls tallwall (draw-dots d123 GameBoard)))))

;game-over? : world → boolean
;takes in a world and checks if pacman has run into a ghost, returns true if he does.
(define (game-over?  world) (pacman-collide-ghosts? (world-APM world)(world-LOG world)))
(check-expect (game-over? world1) #true)

#|###################  Non Game Funcs  ########################################parrot6|#


;count-bigs : real, list-of-real → natnum
;takes in a threshold and a list of numbers, and returns how many of them are larger than the threshold
(define ex1 (cons 1 (cons 2 (cons 3 empty))))
(define ex2 (cons 1 empty))
                  
(define (count-bigs threshold ListNums)(cond [(empty? ListNums) 0]
                                             [(> (first ListNums) threshold) (+ 1 (count-bigs threshold (cdr ListNums)))]
                                             [(<= (first ListNums) threshold) (+ 0 (count-bigs threshold (cdr ListNums)))]
                                             [ else "Call HQ the ship's code is corrupte%!AiDa..2w,36141 kjk9*-+e25 a5wd2 "]))

(check-expect (count-bigs 7 empty) 0)
(check-expect (count-bigs 0 empty) 0)
(check-expect (count-bigs 7 (cons 8 empty)) 1)
(check-expect (count-bigs 0 (cons 0 (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 (cons 8 empty)))))))))) 8)
(check-expect (count-bigs 1 (cons 0 (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 (cons 8 empty)))))))))) 7)
(check-expect (count-bigs 8 (cons 0 (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 (cons 8 empty)))))))))) 0)

;map-sqr : list-of-number → list-of-number
;squares each number in a list
(define (map-sqr ListNums) (cond [(empty? ListNums) '()]
                                 [true (cons (* (first ListNums) (first ListNums)) (map-sqr (cdr ListNums)))]))
(check-expect (map-sqr '()) '())
(check-expect (map-sqr (cons 8 empty)) (cons 64 empty))
(check-expect (map-sqr (cons 0 (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 (cons 6 (cons 7 (cons 8 empty)))))))))) (cons 0 (cons 1 (cons 4 (cons 9 (cons 16 (cons 25 (cons 36 (cons 49 (cons 64 empty))))))))))
(check-expect (map-sqr (cons 0 (cons 1 (cons 2 (cons 3 empty))))) (cons 0 (cons 1 (cons 4 (cons 9 empty)))))


(if (= 1 1) (big-bang world1
              [on-key  world-handle-key]
              [on-tick update-world]
              [to-draw draw-world]) "no auto launch")