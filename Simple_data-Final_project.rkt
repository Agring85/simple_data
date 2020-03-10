;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |space-invaders-v. next-gam, render-game, keyevent|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 50)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

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

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
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

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;; Functions


;; Functions:

;; g -> g
;; start the world with (main G0)
;; 
(define (main G)
  (big-bang G                   
    (on-tick   next-game)         ; Game -> Game
    (to-draw   render-game)       ; Game -> Image
    (on-key    handle-key)        ; Game KeyEvent -> Game
    (stop-when end-game?)))       ; Game -> Boolean

;; Game -> Game
;; Move list of invaders, list of missiles and tank in the game accordingly.

(check-expect (next-game
               (make-game (list (make-invader WIDTH 100 10) (make-invader 100 100 -10)) (list (make-missile 50 50) (make-missile 75 75)) (make-tank 100 1)))
              (make-game (list (make-invader (- WIDTH INVADER-X-SPEED 10) (+ 100 INVADER-Y-SPEED) -10)             
                               (make-invader (- 100 INVADER-X-SPEED 10) (+ 100 INVADER-Y-SPEED) -10))              
                         (list (make-missile 50 (- 50 MISSILE-SPEED)) (make-missile 75 (- 75 MISSILE-SPEED)))      
                         (make-tank (+ 100 TANK-SPEED) 1)))                                                        
(check-expect (next-game
               (make-game (list (make-invader 0 100 -10) (make-invader 100 100 10)) (list (make-missile 150 150) (make-missile 175 175)) (make-tank 100 -1)))
              (make-game (list (make-invader (+ 0 INVADER-X-SPEED 10) (+ 100 INVADER-Y-SPEED) 10)                  
                               (make-invader (+ 100 INVADER-X-SPEED 10) (+ 100 INVADER-Y-SPEED) 10))               
                         (list (make-missile 150 (- 150 MISSILE-SPEED)) (make-missile 175 (- 175 MISSILE-SPEED)))  
                         (make-tank (- 100 TANK-SPEED) -1)))                                                       
                            
;(define (next-game loi lom t) (make-game empty empty T0)) ;stub

(define (next-game G)
  (make-game
   (invaders-onscreen (next-loinvader (add-invader (game-invaders G))) (game-missiles G))
   (missiles-onscreen (next-lom (game-missiles G)) (game-invaders G))
   (next-tank (game-tank G))))

;; List of invaders -> list of invaders
;; randomly add 1 invader, at random x and y = 0 on game-screen

;(define (add-invader loi) empty) ;stub

(define (add-invader loi)
  (if (= (random INVADE-RATE) 1)
      (cons (make-invader (random WIDTH) 0 10) loi)
      loi))

;; list of invaders -> listofinvaders
;; Produce list of invaders that moves an appropriate speed, and bounces of wall

(check-expect (next-loinvader empty) empty)
(check-expect (next-loinvader (list (make-invader 100 100 10))) (list (make-invader (+ 100 INVADER-X-SPEED 10) (+ 100 INVADER-Y-SPEED) 10)))
(check-expect (next-loinvader (list (make-invader 50 50 10) (make-invader WIDTH 100 10)))
              (list (make-invader (+ 50 INVADER-X-SPEED 10) (+ 50 INVADER-Y-SPEED) 10)
                    (make-invader (- WIDTH INVADER-X-SPEED 10) (+ 100 INVADER-Y-SPEED) -10)))

;(define (next-loinvader loi) empty) ; stub

(define (next-loinvader loi)
  (cond [(empty? loi) empty]
        [else
         (cons (next-invader (first loi))
               (next-loinvader (rest loi)))]))

;; invader -> invader
;; Move invader INVADER-X-SPEED and INVADER-Y-SPEED, invader x-coord must be [0, WIDTH], otherwise it bounces of wall

(check-expect (next-invader (make-invader 100 100 -10)) (make-invader (- (- 100 INVADER-X-SPEED) 10) (+ 100 INVADER-Y-SPEED) -10))
(check-expect (next-invader (make-invader WIDTH 50 10)) (make-invader (- WIDTH INVADER-X-SPEED 10) (+ 50 INVADER-Y-SPEED) -10))
(check-expect (next-invader (make-invader 0 50 -10)) (make-invader (+ 0 INVADER-X-SPEED 10) (+ 50 INVADER-Y-SPEED) 10))


(define (next-invader invader)
  (cond [ (>= 0 (invader-x invader))
          (make-invader (+ (invader-x invader) INVADER-X-SPEED (-(invader-dx invader))) (+ (invader-y invader) INVADER-Y-SPEED) (-(invader-dx invader)))]
        [(<= WIDTH (invader-x invader))
         (make-invader (- WIDTH INVADER-X-SPEED (invader-dx invader)) (+ (invader-y invader) INVADER-Y-SPEED) (-(invader-dx invader)))]
        [else
         (move-invader-dir? invader)]))

;; invader -> boolean
;; Produce the next invader that moves right, dx > 0, og left dx < 0.

(check-expect (move-invader-dir? (make-invader 50 50 10)) (make-invader (+ 50 INVADER-X-SPEED 10) (+ 50 INVADER-Y-SPEED) 10))
(check-expect (move-invader-dir? (make-invader 100 75 -10)) (make-invader (- (- 100 INVADER-X-SPEED) 10) (+ 75 INVADER-Y-SPEED) -10))

(define (move-invader-dir? invader)
  (if (> (invader-dx invader) 0)
      (make-invader (+ (invader-x invader) INVADER-X-SPEED (invader-dx invader)) (+ (invader-y invader) INVADER-Y-SPEED) (invader-dx invader))
      (make-invader (- (- (invader-x invader) INVADER-X-SPEED) (- (invader-dx invader))) (+ (invader-y invader) INVADER-Y-SPEED) (invader-dx invader))))

;; List of invader, list of missiles -> list of invaders
;; Produce a list containing only the invaders that are on screen
(check-expect (invaders-onscreen empty empty) empty)
(check-expect (invaders-onscreen (list (make-invader 100 150 10) (make-invader 75 50 -10)) (list (make-missile 100 100) (make-missile 100 50)))
              (list (make-invader 100 150 10) (make-invader 75 50 -10)))
(check-expect (invaders-onscreen (list (make-invader 75 100 10) (make-invader 25 50 -10)) (list (make-missile 100 100) (make-missile (- 25 HIT-RANGE) (+ 50 HIT-RANGE))))
              (list (make-invader 75 100 10)))

;(define (invaders-onscreen loi lom) empty);

(define (invaders-onscreen loi lom)
  (cond [(empty? loi) empty]
        [else
         (if (i-onscreen? (first loi) lom)
             (cons (first loi) (invaders-onscreen (rest loi) lom))
             (invaders-onscreen (rest loi) lom))]))

;; Invader, list of missiles -> boolean
;; Produce true, if invader is not within HIT-RANGE of missile

(check-expect (i-onscreen? (make-invader 150 0 10) empty) true)
(check-expect (i-onscreen? (make-invader 75 100 -10) (list (make-missile 50 25) (make-missile 75 100))) false)
(check-expect (i-onscreen? (make-invader 75 25 10) (list (make-missile 100 50) (make-missile 50 100))) true)

;(define (m-onscreen? i lom) false) ;stub

(define (i-onscreen? i lom)
  (cond
    [(empty? lom) true]
    [(i-no-y-hit? i lom) true] 
    [(and
      (<= (missile-x (first lom)) (+ (invader-x i) HIT-RANGE)) (<= (- (invader-x i) HIT-RANGE) (missile-x (first lom))))
     false]
    [else (i-onscreen? i (rest lom))]))

;; Invader, list of missiles -> boolean
;; Produce true, if y-coord of missile is not within HIT-RANGE of y-coord of invader
(check-expect (i-no-y-hit? (make-invader 75 50 -10) (list (make-missile 100 75) (make-missile 75 100))) true)
(check-expect (i-no-y-hit? (make-invader 100 65 10) (list (make-missile 50 50) (make-missile 25 55))) false)

;(define (i-no-y-hit? i lom) false) ;stub

(define (i-no-y-hit? i lom)
  (cond
    [(empty? lom) true]
    [(and (<= (missile-y (first lom)) (+ (invader-y i) HIT-RANGE)) (>= (missile-y (first lom)) (- (invader-y i) HIT-RANGE)))
     false]
    [else (i-no-y-hit? i (rest lom))]));

;; list of missiles -> list og missiles
;; move missiles in a given list of missiles accordingly

(check-expect (next-lom empty) empty)
(check-expect (next-lom (list (make-missile 100 150))) (list (make-missile 100 (- 150 MISSILE-SPEED))))
(check-expect (next-lom (list (make-missile 100 150) (make-missile 100 200))) (list (make-missile 100 (- 150 MISSILE-SPEED)) (make-missile 100 (- 200 MISSILE-SPEED))))

;(define (next-lom lom) empty) ;stub

(define (next-lom lom)
  (cond [(empty? lom) empty]
        [else
         (cons (next-missiles (first lom))
               (next-lom (rest lom)))]))

;; Missile -> missiles
;; Move given missile accordingly

(check-expect (next-missiles (make-missile 100 100)) (make-missile 100 (- 100 MISSILE-SPEED)))

(define (next-missiles m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))

;; List of missiles, list of invaders -> list of missiles
;; Produce a list containing only the missiles that are on screen


(check-expect (missiles-onscreen empty empty) empty)
(check-expect (missiles-onscreen (list (make-missile 100 (- 0 1))) empty) empty)
(check-expect (missiles-onscreen (list (make-missile 75 50)) (list (make-invader 75 50 10))) empty)
(check-expect (missiles-onscreen (list (make-missile 50 25) (make-missile (- 100 HIT-RANGE) (+ 75 HIT-RANGE))) (list (make-invader 75 100 -10) (make-invader 100 75 10)))
              (list (make-missile 50 25)))

;(define (missiles-onscreen lom loi) empty) ; stub

(define (missiles-onscreen lom loi)
  (cond [(empty? lom) empty]
        [else
         (if (m-onscreen? (first lom) loi)
             (cons (first lom) (missiles-onscreen (rest lom) loi))
             (missiles-onscreen (rest lom) loi))]))

;; List of missiles, list of invader -> boolean
;; Produce false, if missile is within HIT-RANGE of invader, or has gone off the screen, otherwise true.

(check-expect (m-onscreen? (make-missile 150 0) empty) false)
(check-expect (m-onscreen? (make-missile 75 100) (list (make-invader 50 25 -10) (make-invader 75 100 10))) false)
(check-expect (m-onscreen? (make-missile 75 25) (list (make-invader 100 50 -10) (make-invader 50 100 10))) true)

;(define (m-onscreen? m loi) false) ;stub

(define (m-onscreen? m loi)
  (m-no-hit? (m-out m) loi))

;; missile --> missile
;; Keep missile if its not off screen (y > 0), otherwise produce false

(check-expect (m-out (make-missile 50 0)) false)
(check-expect (m-out (make-missile 100 75)) (make-missile 100 75))

;(define (m-out m) empty);stub

(define (m-out m)
  (if (> (missile-y m) 0)
      m
      false))


;; missile --> boolean
;; Produce true, if missile is not within HIT-RANGE of invader, otherwise false.

(check-expect (m-no-hit? (make-missile 75 50) empty) true)
(check-expect (m-no-hit? (make-missile 150 100) (list (make-invader 150 50 -10)(make-invader 50 200 10))) true)
(check-expect (m-no-hit? (make-missile (- 150 HIT-RANGE) (+ 100 HIT-RANGE)) (list (make-invader 100 150 10) (make-invader 150 100 -10))) false)

;(define (m-no-hit? m loi) false) ;stub

(define (m-no-hit? m loi)
  (cond
    [(false? m) false]
    [(empty? loi) true]
    [(m-no-y-hit? m loi) true] 
    [(and
      (<= (missile-x m) (+ (invader-x (first loi)) HIT-RANGE)) (<= (- (invader-x (first loi)) HIT-RANGE) (missile-x m)))
     false]
    [else (m-no-hit? m (rest loi))]))

;; Missile, list of invaders -> boolean
;; Produce true, if y-coord of missile is not within HIT-RANGE of y-coord of invader
(check-expect (m-no-y-hit? (make-missile 75 50) (list (make-invader 100 75 -10) (make-invader 75 100 10))) true)
(check-expect (m-no-y-hit? (make-missile 100 65) (list (make-invader 50 50 10) (make-invader 25 55 10))) false)

;(define (m-no-y-hit? m loi) false) ;stub

(define (m-no-y-hit? m loi)
  (cond
    [(empty? loi) true]
    [(and (<= (missile-y m) (+ (invader-y (first loi)) HIT-RANGE)) (>= (missile-y m) (- (invader-y (first loi)) HIT-RANGE)))
     false]
    [else (m-no-y-hit? m (rest loi))]))

;; tank -> tank
;; Move tank accordingly tank

(check-expect (next-tank (make-tank (/ WIDTH 2) 1))
              (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1)) ; tank goes right
(check-expect (next-tank (make-tank 100 -1))
              (make-tank (- 100 TANK-SPEED) -1))

;(define (next-tank t) (make-tank (/ WIDTH 2) 1)) ;stub

(define (next-tank t)
  (if (> (tank-dir t) 0)
      (make-tank (+ (tank-x t) TANK-SPEED) (tank-dir t))
      (make-tank (- (tank-x t) TANK-SPEED) (tank-dir t))))

;; Game -> Image
;; render an image of tank, list of missiles and list of invaders in their appropriate place on BACKGROUND

(check-expect (render-game (make-game
                            (list (make-invader 50 50 10) (make-invader 100 100 -10))
                            (list (make-missile 75 75) (make-missile 150 150))
                            (make-tank 100 -1)))
              (place-image INVADER 50 50 (place-image INVADER 100 100                                                                                         
                                                      (place-image MISSILE 75 75 (place-image MISSILE 150 150                                                 
                                                                                              (place-image TANK 100 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))))))
;(define (render-game loi lom t) BACKGROUND) ;stub

(define (render-game G)
  (render-space-invaders (game-invaders G) (game-missiles G) (game-tank G)))

;(define (render-space-invaders loi lom t) BACKGROUND)

(define (render-space-invaders loi lom t)
  (cond [(empty? loi) (render-lom lom t)]
        [else
         (render-loi (first loi) (render-space-invaders (rest loi) lom t))
         ]))

;; Invader, image -> image
;; Renders the image of a given invader

(check-expect (render-loi (make-invader 0 0 10) BACKGROUND) (place-image INVADER 0 0 BACKGROUND))
(check-expect (render-loi (make-invader 75 25 -10) BACKGROUND) (place-image INVADER 75 25 BACKGROUND))

;(define (render-loi i img) BACKGROUND) ;stub

(define (render-loi i img)
  (place-image INVADER (invader-x i) (invader-y i) img))


;; list of missiles -> image
;; Render the image of a given list of missiles

(check-expect (render-lom empty (make-tank 100 -1)) (place-image TANK 100 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
(check-expect (render-lom (list (make-missile 150 75)) (make-tank 25 1)) (place-image MISSILE 150 75 (place-image TANK 25 (- HEIGHT TANK-HEIGHT/2) BACKGROUND)))

;(define (render-lom lom t) BACKGROUND) ;stub

(define (render-lom lom t)
  (cond [(empty? lom) (render-tank t)]
        [else
         (render-missile (first lom) (render-lom (rest lom) t))
         ]))

;; missile image -> image
;; Render an image of a given missile

(check-expect (render-missile (make-missile 0 0) BACKGROUND) (place-image MISSILE 0 0 BACKGROUND))
(check-expect (render-missile (make-missile 50 75) BACKGROUND) (place-image MISSILE 50 75 BACKGROUND))

;(define (render-missile m img) BACKGROUND) ;stub

(define (render-missile m img)
  (place-image MISSILE (missile-x m) (missile-y m) img))

;; tank -> image
;; Render the image of tank

(check-expect (render-tank (make-tank 0 1)) (place-image TANK 0 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))
(check-expect (render-tank (make-tank 150 -1)) (place-image TANK 150 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))

;(define (render-tank t) BACKGROUND) ;stub

(define (render-tank t)
  (place-image TANK (tank-x t) (- HEIGHT TANK-HEIGHT/2) BACKGROUND))

;; G keyevent -> G
;; Control the tank by using left-arrow (go left), right-arrow (go right) and space (shoot missiles)

(check-expect (handle-key (make-game empty empty (make-tank 100 1)) "left") (make-game empty empty (make-tank 100 -1)))
(check-expect (handle-key (make-game empty empty (make-tank 100 -1)) "left") (make-game empty empty (make-tank 100 -1)))

(check-expect (handle-key (make-game empty empty (make-tank 75 1)) "right") (make-game empty empty (make-tank 75 1)))
(check-expect (handle-key (make-game empty empty (make-tank 75 1)) "left") (make-game empty empty (make-tank 75 -1)))

(check-expect (handle-key (make-game empty empty (make-tank 50 1)) " ") (make-game empty (list (make-missile 50 (- HEIGHT TANK-HEIGHT/2))) (make-tank 50 1)))
(check-expect (handle-key (make-game empty (list (make-missile 150 100)) (make-tank 100 -1)) " ")
              (make-game empty (list (make-missile 100 (- HEIGHT TANK-HEIGHT/2)) (make-missile 150 100)) (make-tank 100 -1)))

;(define (handle-key G ke) "") ; stub

(define (handle-key G ke)
  (make-game (game-invaders G)
             (missile-keys (game-missiles G) (game-tank G) ke)
             (tank-keys (game-tank G) ke)))

;; lom t ke -> lom
;; Add missile from middle of tank to list of missiles when space (" ") is pressed

(check-expect (missile-keys empty (make-tank 50 1) " ") (list (make-missile 50 (- HEIGHT TANK-HEIGHT/2))))

(check-expect (missile-keys (list (make-missile 75 100) (make-missile 50 50)) (make-tank 50 1) " ")
              (list (make-missile 50 (- HEIGHT TANK-HEIGHT/2)) (make-missile 75 100) (make-missile 50 50)))

(check-expect (missile-keys (list (make-missile 100 150)) T0 "a") (list (make-missile 100 150)))


;(define (missile-keys lom t ke) empty) ;stub

(define (missile-keys lom t ke)
  (cond [(and (empty? lom) (key=? ke " "))
         (cons (make-missile (tank-x t) (- HEIGHT TANK-HEIGHT/2)) empty)]
        [(key=? ke " ")
         (cons (make-missile (tank-x t) (- HEIGHT TANK-HEIGHT/2)) lom)]
        [else
         lom]))

;; tank ke -> tank
;; Move tank by using left-arrow (left) and right-arrow (right)

(check-expect (tank-keys (make-tank 100 1) "left") (make-tank 100 -1))
(check-expect (tank-keys (make-tank 100 -1) "right") (make-tank 100 1))

(check-expect (tank-keys (make-tank 100 1) "right") (make-tank 100 1))
(check-expect (tank-keys (make-tank 100 -1) "left") (make-tank 100 -1))
                         
;(define (tank-keys t ke) T0) ;stub

(define (tank-keys t ke)
  (cond [(and (key=? ke "right") (< (tank-dir t) 0))
         (make-tank (tank-x t) (-(tank-dir t)))]
        [(and (key=? ke "left") (> (tank-dir t) 0))
         (make-tank (tank-x t) (-(tank-dir t)))]
        [else
         t]))

;; Game -> boolean
;; Stops the game, when invaders hits buttom (y = HEIGHT)

(check-expect (end-game? (make-game (list (make-invader 150 75 -10) (make-invader 50 HEIGHT -10)) empty T0)) true)
(check-expect (end-game? (make-game (list (make-invader 75 25 10) (make-invader 50 100 -10)) empty T0)) false)
                                                     
;(define (invader-hit? G) false) ;stub

(define (end-game? G)
  (invader-hit? (game-invaders G)))

;; List of invader -> boolean
;; Produce true, if invader hits buttom (y = HEIGHT)

(check-expect (invader-hit? (list (make-invader 150 75 -10) (make-invader 50 HEIGHT -10))) true)
(check-expect (invader-hit? (list (make-invader 75 25 10) (make-invader 50 100 -10))) false)

;(define (invader-hit? loi) false) ; stub

(define (invader-hit? loi)
  (cond [(empty? loi) false]
        [(<= HEIGHT (invader-y (first loi)))
         true]
        [else
         (invader-hit? (rest loi))]))