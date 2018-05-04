;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders

;; =========================================================================
;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (rectangle WIDTH HEIGHT "outline" "white"))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define MISSILE (ellipse 5 15 "solid" "red"))

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))
(define TANK-WIDTH/2 (/ (image-width TANK) 2))
(define MISSILE-HEIGHT/2 (/ (image-height MISSILE) 2))
(define INVADER-HEIGHT/2 (/ (image-height INVADER) 2))
(define INVADER-WIDTH/2 (/ (image-width INVADER) 2))

(define TANK-Y (- HEIGHT TANK-HEIGHT/2 1))


;; ==========================================================================
;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  ListOfInvader ListOfMissile Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit I1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit I1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit I1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;; ListOfMissile is one of:
;;  - empty
;;  - (cons Missile ListOfMissile)
;; interp. list of missiles

(define LOM1 empty)
(define LOM2 (list M1 M2))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-missile (first lom))
              (fn-for-lom (rest lom)))]))

;; ListOfInvader is one of:
;;  - empty
;;  - (cons Invader ListOfInvader)
;; interp. list of invaders

(define LOI1 empty)
(define LOI2 (list I1 I2))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-invader (first loi))
              (fn-for-loi (rest loi)))]))


(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T2))
(define G2 (make-game (list I1) (list M1) T2))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;; ===============================================================================
;; Functions:

;; Game -> Game
;; start the world with initial game state (main (make-game listOfInvader ListOfMissile Tank)), for example (main G0)

(define (main g)
  (big-bang g                          ; Game
    (on-tick game-tick)                ; Game -> Game
    (to-draw game-render)              ; Game -> Image
    (stop-when game-over?)             ; Game -> Boolean
    (on-key key-handle)))              ; Game KeyEvent -> Game

;; Game -> Game
;; move all invaders, all missiles and tank to their next position in the game according to their speeds and directions
(check-random (game-tick (make-game empty empty (make-tank (/ WIDTH 2) 1)))
              (make-game (add-invader empty) empty (make-tank (+ (/ WIDTH 2) (* 1 TANK-SPEED)) 1)))
(check-random (game-tick (make-game (list (make-invader 150 100 12))
                                    (list (make-missile 150 300))
                                    (make-tank (/ WIDTH 2) -1)))
              (make-game (add-invader (list (make-invader (+ 150 12) (+ 100 12) 12)))
                         (list (make-missile 150 (- 300 MISSILE-SPEED)))
                         (make-tank (+ (/ WIDTH 2) (* -1 TANK-SPEED)) -1)))
(check-random (game-tick (make-game (list (make-invader (- WIDTH INVADER-WIDTH/2) 210 12))
                                    (list (make-missile 55 -15))
                                    (make-tank (- WIDTH TANK-WIDTH/2) 1)))
              (make-game (add-invader (list (make-invader (- (- WIDTH INVADER-WIDTH/2) 12) (+ 210 12) -12)))
                         empty
                         (make-tank (+ (- WIDTH TANK-WIDTH/2) (* -1 TANK-SPEED)) -1)))
(check-random (game-tick (make-game (list (make-invader 44 234 -12) (make-invader 80 100 12))
                                    (list (make-missile 146 200) (make-missile 80 (+ 100 HIT-RANGE)))
                                    (make-tank (/ WIDTH 2) -1)))
              (make-game (add-invader (list (make-invader (- 44 12) (+ 234 12) -12)))
                         (list (make-missile 146 (- 200 MISSILE-SPEED)))
                         (make-tank (+ (/ WIDTH 2) (* -1 TANK-SPEED)) -1)))

;(define (game-tick g) g)               ;Stub

;; <Use template from Game>
(define (game-tick g)
  (make-game (add-invader (advance-invaders (game-invaders g) (game-missiles g)))
             (advance-missiles (game-missiles g) (game-invaders g))
             (advance-tank (game-tank g))))

;; ListofInvader ListofMissile -> ListofInvader
;; add dx value to x coordinate of every Invader and abs(dx) to y coordinate,
;; if x < INVADER-WIDTH/2 or x > (- WIDTH INVADER-WIDTH/2) change sign of dx.
;; Delete invader if its hited with one of missiles
(check-expect (advance-invaders empty empty) empty)
(check-expect (advance-invaders (list (make-invader 44 234 -12) (make-invader 80 100 12))
                                (list (make-missile 146 200) (make-missile 80 (+ 100 HIT-RANGE))))
              (list (make-invader (- 44 12) (+ 234 12) -12)))
(check-expect (advance-invaders (list (make-invader 56 210 -12) (make-invader (- WIDTH INVADER-WIDTH/2) 210 12) (make-invader INVADER-WIDTH/2 125 -12)) empty)
              (list (make-invader (- 56 12) (+ 210 12) -12) (make-invader (- (- WIDTH INVADER-WIDTH/2) 12) (+ 210 12) -12) (make-invader (+ INVADER-WIDTH/2 12) (+ 125 12) 12)))

;(define (advance-invaders loi lom) loi) ;Stub
 
(define (advance-invaders loi lom)
  (cond [(empty? loi) empty]
        [else
         (if (shotdown? (first loi) lom)
             (advance-invaders (rest loi) lom)
             (cons (advance-invader (first loi))
                   (advance-invaders (rest loi) lom)))]))


;; Invader (listof Missile) -> Boolean
;; produce true if invader x y coordinates collide with x y coordinate of one of the missiles
(check-expect (shotdown? (make-invader 50 230 10) empty) false)
(check-expect (shotdown? (make-invader 50 230 10)
                      (list (make-missile 89 123)
                            (make-missile (+ 50 HIT-RANGE) (+ 230 HIT-RANGE))))
                      true)

;(define (shotdown? i lom) false)          ;Stub

(define (shotdown? i lom)
  (cond [(empty? lom) false]
        [else
         (if (collide? i (first lom))
             true
             (shotdown? i (rest lom)))]))

;; Invader Missile -> Boolean
;; produce true if invader and missile collides
(check-expect (collide? (make-invader 50 230 10) (make-missile 89 123)) false)
(check-expect (collide? (make-invader 50 230 10) (make-missile (- 50 HIT-RANGE) (+ 230 HIT-RANGE))) true)
(check-expect (collide? (make-invader 50 230 10) (make-missile (+ 50 HIT-RANGE) (- 230 HIT-RANGE))) true)
(check-expect (collide? (make-invader 50 230 10) (make-missile (+ 50 HIT-RANGE) (+ 230 HIT-RANGE))) true)
(check-expect (collide? (make-invader 50 230 10) (make-missile (- 50 HIT-RANGE) (- 230 HIT-RANGE))) true)
(check-expect (collide? (make-invader 50 230 10) (make-missile (+ 50 (+ 1 HIT-RANGE)) (+ 230 HIT-RANGE))) false)
(check-expect (collide? (make-invader 50 230 10) (make-missile (+ 50 HIT-RANGE) (+ 230 (+ 1 HIT-RANGE)))) false)

;(define (collide? i m) false)          ;Stub

(define (collide? i m)
  (and (>= (invader-x i) (- (missile-x m) HIT-RANGE))
       (<= (invader-x i) (+ (missile-x m) HIT-RANGE))
       (>= (invader-y i) (- (missile-y m) HIT-RANGE))
       (<= (invader-y i) (+ (missile-y m) HIT-RANGE))))


;; Invader -> Invader
;; add dx value to x coordinate of Invader and abs(dx) to y coordinate,
;; if x < INVADER-WIDTH/2 or x > (- WIDTH INVADER-WIDTH/2) change sign of dx
(check-expect (advance-invader (make-invader 50 230 10)) (make-invader (+ 50 10) (+ 230 10) 10))
(check-expect (advance-invader (make-invader INVADER-WIDTH/2 120 -5)) (make-invader (+ INVADER-WIDTH/2 5) (+ 120 5) 5))
(check-expect (advance-invader (make-invader (- WIDTH INVADER-WIDTH/2) 65 10)) (make-invader (- (- WIDTH INVADER-WIDTH/2) 10) (+ 65 10) -10))

;(define (advance-invader i) i)        ;Stub

(define (advance-invader i)
  (if (<= INVADER-WIDTH/2 (+ (invader-x i) (invader-dx i)) (- WIDTH INVADER-WIDTH/2))
      (make-invader (+ (invader-x i) (invader-dx i)) (+ (invader-y i) (abs (invader-dx i))) (invader-dx i))
      (make-invader (- (invader-x i) (invader-dx i)) (+ (invader-y i) (abs (invader-dx i))) (- (invader-dx i)))))


;; (listof Missile) (listof Invader) -> (listof Missile)
;; decrease y value of every Missile by MISSILE-SPEED
;; Delete missile if it hit one of the invaders or if y < -MISSILE-HEIGHT/2
(check-expect (advance-missiles empty empty) empty)
(check-expect (advance-missiles (list (make-missile 50 200) (make-missile 125 145) (make-missile 10 53)) (list (make-invader 57 25 -3) (make-invader 11 51 3)))
              (list (make-missile 50 (- 200 MISSILE-SPEED)) (make-missile 125 (- 145 MISSILE-SPEED))))
(check-expect (advance-missiles (list (make-missile 120 34) (make-missile 35 -10)) (list (make-invader 57 25 -3) (make-invader 11 51 3)))
              (list (make-missile 120 (- 34 MISSILE-SPEED))))

;(define (advance-missiles lom loi) lom) ;Stub

(define (advance-missiles lom loi)
  (cond [(empty? lom) empty]
        [else
         (if (or (hitted? (first lom) loi) (out? (first lom)))
             (advance-missiles (rest lom) loi)
             (cons (advance-missile (first lom)) (advance-missiles (rest lom) loi)))]))


;; Missile ListOfInvader -> Boolean
;; produce true if missile collide with one of the invaders
(check-expect (hitted? (make-missile 58 32) empty) false)
(check-expect (hitted? (make-missile 89 120) (list (make-invader 56 234 2) (make-invader 87 122 2))) true)
(check-expect (hitted? (make-missile 120 33) (list (make-invader 45 243 -3) (make-invader 89 23 3))) false)

;(define (hitted? m loi) false)          ;Stub

(define (hitted? m loi)
  (cond [(empty? loi) false]
        [else
         (if (collide? (first loi) m)
             true
             (hitted? m (rest loi)))]))

;; Missile -> Boolean
;; produce true if y coordinate of missile < -MISSILE-HEIGHT/2 
(check-expect (out? (make-missile 125 59)) false)
(check-expect (out? (make-missile 58 -20)) true)
(check-expect (out? (make-missile 45 (- MISSILE-HEIGHT/2))) false)

;(define (out? m) false)                 ;Stub

(define (out? m)
  (< (missile-y m) (- MISSILE-HEIGHT/2)))

;; Missile -> Missile
;; decrease y coordinate of Missile by MISSILE-SPEED
(check-expect (advance-missile (make-missile 67 145)) (make-missile 67 (- 145 MISSILE-SPEED)))
(check-expect (advance-missile (make-missile 125 25)) (make-missile 125 (- 25 MISSILE-SPEED)))

;(define (advance-missile m) m)          ;Stub

(define (advance-missile m)
  (make-missile (missile-x m)
                (- (missile-y m) MISSILE-SPEED)))

;; Tank -> Tank
;; add (* TANK-SPEED dx) to x coordinate of tank
;; if x < TANK-WIDTH/2 or x > (- WIDTH TANK-WIDTH/2) change sign of dx
(check-expect (advance-tank (make-tank 120 1)) (make-tank (+ 120 (* TANK-SPEED  1))  1))
(check-expect (advance-tank (make-tank 54 -1)) (make-tank (+ 54 (* TANK-SPEED -1)) -1))
(check-expect (advance-tank (make-tank TANK-WIDTH/2 -1)) (make-tank (+ TANK-WIDTH/2 (* TANK-SPEED 1)) 1))
(check-expect (advance-tank (make-tank (- WIDTH TANK-WIDTH/2) 1)) (make-tank (+ (- WIDTH TANK-WIDTH/2) (* TANK-SPEED -1)) -1))

;(define (advance-tank t) t)             ;Stub

(define (advance-tank t)
  (if (< TANK-WIDTH/2 (+ (tank-x t) (* TANK-SPEED (tank-dir t))) (- WIDTH TANK-WIDTH/2))
      (make-tank (+ (tank-x t) (* TANK-SPEED (tank-dir t))) (tank-dir t))
      (make-tank (+ (tank-x t) (* TANK-SPEED (- (tank-dir t)))) (- (tank-dir t)))))

;; (listof Invader) -> (listof Invader)
;; randomly add new invader to list
(check-random (add-invader empty) (if (= (random INVADE-RATE) 1)
                                      (list (make-invader (random WIDTH) 0 INVADER-X-SPEED))
                                      empty))
(check-random (add-invader (list (make-invader 100 150 -2) (make-invader 50 50 2)))
              (if (= (random INVADE-RATE) 1)
                  (cons (make-invader (random WIDTH) 0 INVADER-X-SPEED)
                        (list (make-invader 100 150 -2) (make-invader 50 50 2)))
                  (list (make-invader 100 150 -2) (make-invader 50 50 2))))

;(define (add-invader loi) loi)         ;Stub

(define (add-invader loi)
  (if (= (random INVADE-RATE) 1)
      (cons (make-invader (random WIDTH) 0 INVADER-X-SPEED) loi)
      loi))

;; Game -> Image
;; render all invaders, all missiles and tank on background in appropriate positions
(check-expect (game-render (make-game empty empty (make-tank (/ WIDTH 2) 1)))
              (place-image TANK (/ WIDTH 2) TANK-Y BACKGROUND))
(check-expect (game-render (make-game (list (make-invader 100 50 -3) (make-invader 50 200 3))
                                      (list (make-missile 45 30) (make-missile 125 200))
                                      (make-tank 150 1)))
              (place-image INVADER 100 50
                           (place-image INVADER 50 200
                                        (place-image MISSILE 45 30
                                                     (place-image MISSILE 125 200
                                                                  (place-image TANK 150 TANK-Y BACKGROUND))))))
;(define (game-render g) BACKGROUND)    ;Stub

(define (game-render g)
  (overlay (render-invaders (game-invaders g))
           (render-missiles (game-missiles g))
           (render-tank (game-tank g))))

;; ListOfInvaders -> Image
;; create image of all invaders in appropriate place according to their coordinates
(check-expect (render-invaders empty) (square 0 "solid" "white"))
(check-expect (render-invaders (list (make-invader 50 200 -3) (make-invader 124 53 2) (make-invader 76 250 1)))
              (place-image INVADER 50 200
                           (place-image INVADER 124 53
                                        (place-image INVADER 76 250 BACKGROUND))))

;(define (render-invaders loi) (square 0 "solid" "white"))    ;Stub

(define (render-invaders loi)
  (cond [(empty? loi) (square 0 "solid" "white")]
        [else
         (overlay (render-invader (first loi))
                  (render-invaders (rest loi)))]))

;; Invader -> Image
;; create image of invader in appropriate place on BACKGROUND
(check-expect (render-invader (make-invader 100 25 3)) (place-image INVADER 100 25 BACKGROUND))

;(define (render-invader i) BACKGROUND)                     ;Stub
(define (render-invader i)
  (place-image INVADER (invader-x i) (invader-y i) BACKGROUND))


;; ListOfMissiles -> Image
;; create image of all missiles in appropriate place
(check-expect (render-missiles empty) (square 0 "solid" "white"))
(check-expect (render-missiles (list (make-missile 65 34) (make-missile 123 56)))
              (place-image MISSILE 65 34
                           (place-image MISSILE 123 56 BACKGROUND)))

;(define (render-missiles lom) (square 0 "solid" "white"))    ;Stub

(define (render-missiles lom)
  (cond [(empty? lom) (square 0 "solid" "white")]
        [else
         (overlay (render-missile (first lom))
                  (render-missiles (rest lom)))]))

;; Missile -> Image
;; create image of missile on the BACKGROUND at appropriate place
(check-expect (render-missile (make-missile 45 234)) (place-image MISSILE 45 234 BACKGROUND))

;(define (render-missile m) BACKGROUND)                     ;Stub
                        
(define (render-missile m)
  (place-image MISSILE (missile-x m) (missile-y m) BACKGROUND))


;; Tank -> Image
;; create image of tank on the BACKGROUND in position according to the coordinates
(check-expect (render-tank (make-tank 76 -1)) (place-image TANK 76 TANK-Y BACKGROUND))

;(define (render-tank t) BACKGROUND)                         ;Stub

(define (render-tank t)
  (place-image TANK (tank-x t) TANK-Y BACKGROUND))

;; Game -> Boolean
;; produce true if y coordinate of one of the invaders reaches value HEIGHT
(check-expect (game-over? (make-game empty empty T1)) false)
(check-expect (game-over? (make-game (list (make-invader 45 98 3) (make-invader 67 43 2) (make-invader 56 HEIGHT -3)) empty T1)) true)
(check-expect (game-over? (make-game (list (make-invader 177 23 -3) (make-invader 56 (- HEIGHT INVADER-HEIGHT/2) 3)) empty T1)) true)
(check-expect (game-over? (make-game (list (make-invader 177 23 -3) (make-invader 56 (- HEIGHT (+ 1 INVADER-HEIGHT/2)) 3)) empty T1)) false)

;(define (game-over? g) false)             ;Stub

(define (game-over? s)
  (invaders-landed? (game-invaders s)))

;; ListOfInvaders -> Boolean
;; produce true if y coordinate of one of the invaders toches bottom of screen
(check-expect (invaders-landed? empty) false)
(check-expect (invaders-landed? (list (make-invader 45 98 3) (make-invader 67 43 2) (make-invader 56 HEIGHT -3))) true)
(check-expect (invaders-landed? (list (make-invader 45 98 3) (make-invader 67 43 2))) false)
(check-expect (invaders-landed? (list (make-invader 177 23 -3) (make-invader 56 (- HEIGHT INVADER-HEIGHT/2) 3))) true)

;(define (invaders-landed? loi) false)    ;Stub

(define (invaders-landed? loi)
  (cond [(empty? loi) false]
        [else
         (if (invader-landed? (first loi))
             true
             (invaders-landed? (rest loi)))]))

;; Invader -> Boolean
;; produce true if invader toches bottom of the screen
(check-expect (invader-landed? (make-invader 45 65 5)) false)
(check-expect (invader-landed? (make-invader 56 (- HEIGHT INVADER-HEIGHT/2) 3)) true)

;(define (invader-landed? i) false)      ;Stub

(define (invader-landed? i)
  (>= (invader-y i) (- HEIGHT INVADER-HEIGHT/2)))
      

;; Game KeyEvent -> Game
;; Change direction of the tank to 1 if right arrow key pressed ot to -1 if left arrow key pressed
;; create new missile if space bar pressed
(check-expect (key-handle (make-game empty empty (make-tank 64 -1)) "left") (make-game empty empty (make-tank 64 -1)))
(check-expect (key-handle (make-game empty empty (make-tank 64 -1)) "right") (make-game empty empty (make-tank 64 1)))
(check-expect (key-handle (make-game empty empty (make-tank 64 1)) "left") (make-game empty empty (make-tank 64 -1)))
(check-expect (key-handle (make-game empty empty (make-tank 64 1)) "right") (make-game empty empty (make-tank 64 1)))
(check-expect (key-handle (make-game empty (list (make-missile 34 300) (make-missile 56 78)) (make-tank 120 1)) " ")
              (make-game empty (list (make-missile 120 (- HEIGHT (* 2 TANK-HEIGHT/2) MISSILE-HEIGHT/2)) (make-missile 34 300) (make-missile 56 78)) (make-tank 120 1)))
(check-expect (key-handle (make-game empty (list (make-missile 34 300) (make-missile 56 78)) (make-tank 120 1)) "g")
              (make-game empty (list (make-missile 34 300) (make-missile 56 78)) (make-tank 120 1)))

;(define (key-handle g ke) g)             ;Stub

(define (key-handle g ke)
  (cond [(key=? ke "left")  (make-game (game-invaders g) (game-missiles g) (make-tank (tank-x (game-tank g)) -1))]
        [(key=? ke "right") (make-game (game-invaders g) (game-missiles g) (make-tank (tank-x (game-tank g)) 1))]
        [(key=? ke " ")     (make-game (game-invaders g) (add-missile (game-missiles g) (game-tank g)) (game-tank g))]
        [else g]))

;; ListOfMissiles Tank -> ListOfMissiles
;; create new missile at the position of tank
(check-expect (add-missile (list (make-missile 76 300) (make-missile 120 45)) (make-tank 20 -1))
              (list (make-missile 20 (- HEIGHT (* 2 TANK-HEIGHT/2) MISSILE-HEIGHT/2)) (make-missile 76 300) (make-missile 120 45)))
              
;(define (add-missile lom t) lom)      ;Stub

(define (add-missile lom t)
  (cons (make-missile (tank-x t)
                      (- HEIGHT (* 2 TANK-HEIGHT/2) MISSILE-HEIGHT/2))
        lom))



