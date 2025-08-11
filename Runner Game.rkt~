;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |Runner Game|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define background (place-image (rectangle 700 1 "solid" "black") 350 150 (empty-scene 700 200)))

; A Object is one of:
; - "tall"
; - "regular"
; - "long"
; - "flying"

(define tall (rectangle 30 50 "solid" "orange"))
(define regular (regular-polygon 30 4 "solid" "blue"))
(define long (rectangle 50 30 "solid" "red"))
(define flying (star 20 "solid" "magenta"))

; A State is one of:
; - "neutral"
; - "ducking"
; - "jumping"
; - "landing"

(define neutral (rectangle 30 50 "solid" "red"))
(define ducking (rectangle 50 30 "solid" "red"))

; A obstacle is (make-obstacle Object Number Posn)
(define-struct obstacle [type x-vel pos])

; A player is (make-player Number Number state)
(define-struct player [y y-vel status])

; A World is (make-world Player ListOfObstacle NaturalNumber)
(define-struct world [player obstacles score])

(define starting-world (make-world (make-player 135 0 "neutral")
                                   (list (make-obstacle "regular" 5 (make-posn 700 135))
                                         (make-obstacle "long" 5 (make-posn 1200 135))
                                         (make-obstacle "flying" 5 (make-posn 1700 85)))
                                   0))

; draw : World -> Image
; draws the game
(define (draw w)
  (foldr (lambda (o img) (place-image (draw-obstacle o) (posn-x (obstacle-pos o)) (posn-y (obstacle-pos o)) img))
         (place-image  (draw-score (world-score w) 4)
                       665 15 (draw-player (world-player w)))
         (world-obstacles w)))

; draw-obstacle : Obstacle -> Image
; draws an obstacle based on its type
(check-expect (draw-obstacle (make-obstacle "tall" 5 (make-posn 750 125))) tall)
(check-expect (draw-obstacle (make-obstacle "regular" 5 (make-posn 750 135))) regular)
(check-expect (draw-obstacle (make-obstacle "long" 5 (make-posn 750 135))) long)
(check-expect (draw-obstacle (make-obstacle "flying" 5 (make-posn 750 120))) flying)
(define (draw-obstacle o)
  (cond [(string=? (obstacle-type o) "tall") tall]
        [(string=? (obstacle-type o) "regular") regular]
        [(string=? (obstacle-type o) "long") long]
        [else flying]))

; draw-player : Player -> Image
; draws the player on the background
(check-expect (draw-player (make-player 135 0 "ducking"))
              (place-image (rectangle 50 30 "solid" "grey")
                      35 145 background))
(check-expect (draw-player (make-player 60 15 "flying"))
              (place-image (rectangle 30 50 "solid" "grey")
                      30 60 background))
(define (draw-player p)
  (cond [(string=? (player-status p) "ducking")
         (place-image (rectangle 50 30 "solid" "grey")
                      35 145 background)]
        [else
         (place-image (rectangle 30 50 "solid" "grey")
                      30 (player-y p) background)]))

; draw-score : NaturalNumber NaturalNumber -> Image
; draws the score with leading 0s if value is smaller than limit
(define (draw-score score n)
  (cond [(= score 0) (text "00000" 20 "black")]
        [(>= (- score (expt 10 n)) 0) (text (number->string (floor score)) 20 "black")]
        [else (beside (text "0" 20 "black") (draw-score score (sub1 n)))]))

; move-player : World KeyEvent -> World
; moves the player jump or makes them duck based on user input
; ^on-key handler
(define (move-player w ke)
  (local [(define status (player-status (world-player w)))]
  (cond [(and (or (string=? ke "up") (string=? ke " ")) (string=? status "neutral"))
         (make-world (make-player (player-y (world-player w)) -12.25 "jumping")
                     (world-obstacles w)
                     (world-score w))]
        [(and (string=? (player-status (world-player w)) "jumping") (string=? ke "down"))
         (make-world (make-player (player-y (world-player w)) (+ (player-y-vel (world-player w)) 5) "landing")
                     (world-obstacles w)
                     (world-score w))]
        [(and (string=? ke "down") (string=? status "neutral"))
         (make-world (make-player 145 (player-y-vel (world-player w)) "ducking")
                     (world-obstacles w)
                     (world-score w))]
        [else w]))) ; disregards other KeyEvents

; stand-or-jump : World KeyEvent -> World
; if a player is ducking, makes them stop and if a player is jumping, makes them jump higher
(define (stand-or-jump w ke)
  (cond [(and (string=? (player-status (world-player w)) "ducking") (string=? ke "down"))
         (make-world (make-player 135 0 "neutral")
                     (world-obstacles w)
                     (world-score w))]
        [(and (string=? (player-status (world-player w)) "landing") (string=? ke "down") (< (player-y (world-player w)) 135))
         (make-world (make-player (player-y (world-player w)) (player-y-vel (world-player w)) "jumping")
                     (world-obstacles w)
                     (world-score w))]
        [(or (string=? ke "up") (string=? ke " "))
         ; I'm guessing the function times out after a long press, thus a long press doesn't increase the y-vel
         (make-world (make-player (player-y (world-player w)) (+ (player-y-vel (world-player w)) 2.25) "jumping")
                     (world-obstacles w)
                     (world-score w))]
        [else w]))

; update : World -> World
; increments score, moves obstacles towards player, and removes past obstacles
(define (update w)
  (make-world (update-player (world-player w))
              (generate-obstacles (update-obstacles (world-obstacles w)) (world-score w))
              (+ 0.2 (world-score w))))

; update-player : Player -> Player
; changes the player height based on y-vel, then increases y-vel
(define (update-player p)
  (gravity (make-player (min 135 (+ (player-y p) (player-y-vel p)))
                        (player-y-vel p)
                        (player-status p))))

; gravity : Player -> Player
; increases y-vel and updates player status if ground is hit
(define (gravity p)
  (cond [(and (string=? (player-status p) "landing") (= (player-y p) 135))
         (make-player 145 0 "ducking")]
        [(not (or (string=? (player-status p) "jumping") (string=? (player-status p) "landing"))) p]
        [(= (player-y p) 135)
         (make-player 135 0 "neutral")]
        [else (make-player (player-y p) (+ (player-y-vel p) 0.725) (player-status p))]))

; update-obstacles : ListOfObstacles -> ListOfObstacles
; moves ostacles and removes off-screen ones
(define (update-obstacles loo)
  (map (lambda (o) (make-obstacle (obstacle-type o) (obstacle-x-vel o)
                                  (make-posn (- (posn-x (obstacle-pos o)) (obstacle-x-vel o))
                                             (posn-y (obstacle-pos o)))))
       (if (< (posn-x (obstacle-pos (first loo))) -20) (rest loo) loo)))

; generate-obstacles : ListOfObstacles NaturalNumber -> ListofObstacles
; generates obstacles if there are only 2 left
(define (generate-obstacles loo score)
  (cond [(and (= (length loo) 1) (> 100 (posn-x (obstacle-pos (first loo)))))
         (append loo (list (generate-obstacle (random 4) score)
                           (adjust-spacing (generate-obstacle (random 4) score) (max (random 120) 80))
                           (adjust-spacing (generate-obstacle (random 4) score) (* 2 (max (random 120) 80)))))]
        [else loo]))
                    

; generate-obstacle : NaturalNumber NaturalNumber -> Obstacle
; generates one of four obstacles
(define (generate-obstacle type score)
  (local [(define n (random 3))]
  (cond [(or (not (= type 0)) (> 300 score))
         (make-obstacle (list-ref (list "tall" "regular" "long") n)
                        (min (+ (quotient (floor score) 400) 5) 13)
                        (make-posn 720 (list-ref (list 125 135 135) n)))]
        [else (make-obstacle "flying"
                             (min (+ (quotient (floor score) 400) 5) 13)
                             (make-posn 720 (list-ref (list 85 105 135) n)))])))

; adjust-spacing : Obstacle -> Obstacle
; increases obstacle-x by n to ensure player can jump over
(define (adjust-spacing o n)
  (make-obstacle (obstacle-type o) (obstacle-x-vel o)
                 (make-posn (+ (* n (obstacle-x-vel o)) (posn-x (obstacle-pos o)))
                            (posn-y (obstacle-pos o)))))
 
; end : World -> Boolean
; ends the game
(define (end w)
  (or (collision? w) (= 99999 (world-score w))))

; collision? : World -> Boolean
; checks if there is a collision
(define (collision? w)
  (ormap (lambda (o) (hit? o (player-y (world-player w)) (player-status (world-player w)))) (world-obstacles w)))

; hit? : Obstacle Number State -> Boolean
; determines if a player hit an obstacle based on distance and obstacle size
(define (hit? o y s)
  (local [(define x-dist (abs (- (posn-x (obstacle-pos o)) 30)))
          (define y-dist (abs (- (posn-y (obstacle-pos o)) y)))]
  (cond [(string=? s "ducking")
         (ground-hit? o (abs (- (posn-x (obstacle-pos o)) 35))
                      (abs (- (posn-y (obstacle-pos o)) y)) 40 50 25)]
        [(string=? s "neutral")
         (ground-hit? o x-dist y-dist 30 40 45)]
        [else
         (air-hit? o x-dist y-dist)])))

; ground-hit? : Obstacle Number Number Number Number Number-> Boolean
; checks if the player has hit an obstacle while on the ground
(define (ground-hit? o x-dist y-dist n1 n2 n3)
  (cond [(or (string=? (obstacle-type o) "tall")
             (string=? (obstacle-type o) "regular"))
         (>= n1 x-dist)]
        [(string=? (obstacle-type o) "long")
         (>= n2 x-dist)]
        [(string=? (obstacle-type o) "flying")
         (and (>= 35 x-dist)
              (>= n3 y-dist))]))

; air-hit? : Obstacle Number Number -> Boolean
; checks if the player has hit an obstacle while in the air
(define (air-hit? o x-dist y-dist)
  (cond [(string=? (obstacle-type o) "tall") ; 15 = x and 25 = y
         (and (>= 30 x-dist)
              (>= 50 y-dist))]
        [(string=? (obstacle-type o) "regular")
         (and (>= 30 x-dist)
              (>= 40 y-dist))]
        [(string=? (obstacle-type o) "long")
         (and (>= 40 x-dist)
              (>= 40 y-dist))]
        [(string=? (obstacle-type o) "flying")
         (and (>= 35 x-dist)
              (>= 45 y-dist))]))

; final-scene : World -> Image
; draws the ending scene with game-over written on it
(define (final-scene w)
  (overlay (text "Game Over" 40 "black") (draw w)))

; run : World -> World
; runs the game
(define (run start)
  (big-bang start
    [to-draw draw]
    [on-key move-player]
    [on-release stand-or-jump]
    [on-tick update (/ 1 60)]
    [stop-when end final-scene]
    [display-mode 'normal]
    [name "Dhruv's Game"]))

(run starting-world)

                           