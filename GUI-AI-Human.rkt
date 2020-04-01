#lang racket/gui
(require "constants.rkt")

(require "classes.rkt")
(require 2htdp/universe)
(require 2htdp/image)
(require lang/posn)

;---------------------GRAPHIC ELEMENTS-----------------------------;
(define emp-scene (empty-scene 920 690 "white"))                     ;EMPTY SCENE WHERE ALL THE IMAGES WILL BE DISPLAYED
(define bitmp (bitmap/file "ludo_690.jpg"))                         
(define scene-ludo (place-image bitmp 460 345 emp-scene))
(define home-bitmap (bitmap/file "start_window.png"))
(define home-screen (place-image home-bitmap 460 345 emp-scene))     ;LUDO BOARD
(define safe-places (star 25 "outline" "black"))                     ;STARS FOR SHOWING SAFE PLACES
(define list-safe-place (build-list 8 (lambda (x) safe-places)))     ;LIST OF SAFE PLACES
(define no-of-player-page (bitmap/file "player_window.png"))         ;SCREEN FOR DECIDING NO. OF PLAYERS
(define type-of-player-page (bitmap/file "player1_window.jpg"))      ;DECIDES WHETHER THE PLAYER WILL BE AI OR HUMAN
(define choose-colour-page-1 (bitmap/file "color_window1.jpg"))
(define choose-colour-page-2 (bitmap/file "color_window2.jpg"))
(define choose-colour-page-3 (bitmap/file "color_window3.jpg"))
(define choose-colour-page-4 (bitmap/file "color_window4.jpg"))
(define screen 'undefined)
(define (button-sounds) (play-sound "button-sound.wav" #f))
(define (dice-sounds) (play-sound "dice-sound.wav" #f))
(define scene (empty-scene 920 690 "white"))
(define state 'home)                                                 ; THE STATE VARIABLE OF THE GAME
(define (game-over sce) (place-image (bitmap/file "gameover.jpg") 460 345 emp-scene)) ;SCREEN DISPLAYED WHEN THE GAME IS OVER

;---------------------------------PLAYER SETTING FUNCTIONS-----------------------; 
(define(player-type-setter x)                                                    ; SETS THE TYPE OF PLAYERS
 (if(equal? x 'AI-player) (set-field! player-types ludo (cons (list 'human 'undefined 'undefined)
                                                            (map (lambda(x) (list 'AI-player 'undefined 'undefined)) (cdr (get-field player-types ludo)))))
    (set-field! player-types ludo (map (lambda(x) (list 'human 'undefined 'undefined)) (get-field player-types ludo)))))

(define(colour-setter-AI c l)                                                    ;SETS THE COLORS AS CHOSEN BY THE HUMAN OR RANDOM IN CASE OF AI
    (begin (set-field! player-types ludo (cons (list 'human c 'undefined)
   (begin (set! l  (remove c l))
     (map (lambda(x) (let ([c1 (list-ref l (random (length l)))])
                       (begin (set! l (remove c1 l)) (list 'AI-player c1 'undefined)))) (cdr (get-field player-types ludo))))))))

(define (colour-setter-human-2-1 c)                                            ; VARIOUS COLOR SETTING FUNCTIONS FOR DIFFERENT NO. OF PLAYERS
  (set-field! player-types ludo (cons (list 'human c 'undefined) (cdr (get-field player-types ludo)))))
(define (colour-setter-human-2-2 c)
  (set-field! player-types ludo (append (list (car (get-field player-types ludo))) (list (list 'human c 'undefined)))))
(define (colour-setter-human-3-1 c)
  (set-field! player-types ludo (cons (list 'human c 'undefined) (cdr (get-field player-types ludo)))))
(define (colour-setter-human-3-2 c)
  (set-field! player-types ludo (append (list (car (get-field player-types ludo))) (list (list 'human c 'undefined)) (list (list 'human 'undefined 'undefined)))))
(define (colour-setter-human-3-3 c)
 (set-field! player-types ludo (append (reverse (cdr (reverse (get-field player-types ludo)))) (list (list 'human c 'undefined)))))
(define (colour-setter-human-4-1 c)
  (set-field! player-types ludo (cons (list 'human c 'undefined) (cdr (get-field player-types ludo)))))
(define (colour-setter-human-4-2 c)
  (set-field! player-types ludo (append (list (car (get-field player-types ludo))) (list (list 'human c 'undefined)) (list (list 'human 'undefined 'undefined)) (list (list 'human 'undefined 'undefined)))))
(define (colour-setter-human-4-3 c)
 (set-field! player-types ludo (append (list (car (get-field player-types ludo))) (list (cadr (get-field player-types ludo))) (list (list 'human c 'undefined)) (list (list 'human 'undefined 'undefined)))))
(define (colour-setter-human-4-4 c)
  (set-field! player-types ludo (append (reverse (cdr (reverse (get-field player-types ludo)))) (list (list 'human c 'undefined)))))

;--------------------------HELPER FUNCTIONS---------------------------;
(define (calc x)
  (let* ([q (quotient x a)]
         [res (+ (* q a) (/ a 2))])
    res))
(define (square-centre x y)                                          ;GIVES THE CENTRE OF THE SQUARE
  (let ([x1 (- x add-constant)])
    (cons (+ (calc x1) add-constant) (calc y))))
 (define (f sce)                                                     ;RETURNS THE NUMBER OF ACTIVE PLAYERS IN THE GAME
    (send ludo length-active-players))
(define(circle-in centre-x centre-y r x y)                           ;INTERIOR REGION OF A CIRCLE
  (if(<= (+ (expt (- x centre-x) 2) (expt (- y centre-y) 2)) (expt r 2)) #t #f))

;-------------------------HOME SCREEN------------------------------------;
(define (start-region x y)                                           ; VALID REGION FOR CLICKING ON THE START BUTTON
  (if (and (and (< x 666) (> x 330)) (and (> y 501) (< y 646))) #t #f))
(define (start-click sce x y mouse-pos)                              ; DETECTS MOUSE CLICK AND CHANGES PROGRAM STATE
  (if (and (equal? mouse-pos "button-down") (start-region x y)) (begin (button-sounds) (set! state 'players) sce)
      sce))
;-------------------------------PLAYERS SCREEN------------------------------;
(define players-screen                                               ;SCREEN FOR SELECTING NUMBER OF PLAYERS
  (place-image no-of-player-page (car centre-board) (cdr centre-board) scene))
(define (players sce x y mouse-pos)
  (if (equal? mouse-pos "button-down")
      (cond [(2-players x y) (begin (button-sounds) (set-field! player-types ludo (make-list 2 '())) (set! state 'type-of-players) sce)]
            [(3-players x y) (begin (button-sounds) (set-field! player-types ludo (make-list 3 '())) (set! state 'type-of-players) sce)]
            [(4-players x y) (begin (button-sounds) (set-field! player-types ludo (make-list 4 '())) (set! state 'type-of-players) sce)])
      sce))
  
(define(2-players x y)
  (circle-in 214 262 55 x y))
(define(3-players x y)
  (circle-in 217 403 55 x y))
(define(4-players x y)
  (circle-in 218 545 52 x y))            
  
;-------------------------------TYPE OF PLAYERS SCREEN----------------------;
(define player-type-screen                                                   ;SCREEN FOR SELECTING THE TYPE OF PLAYER
  (place-image  type-of-player-page (car centre-board) (cdr centre-board) scene))
(define (type-of-players sce x y mouse-pos)
  (if (equal? mouse-pos "button-down")
  (cond [(computer x y) (begin (button-sounds) (player-type-setter 'AI-player)  (set! state 'colour-choose) sce)]
        [(human x y) (begin (button-sounds) (player-type-setter 'human) (set! state 'colour-choose) sce)]) sce))
(define(computer x y)                                                       ;COMPUTER PLAYER IS SELECTED BY CLICKING IN THIS REGION
  (if(and (> x 88) (< x 419) (> y 279) (< y 522)) #t #f))
(define(human x y)                                                          ; HUMAN PLAYER IS SELECTED BY CLICKING IN THIS REGION
  (if(and (> x 502) (< x 837) (> y 277) (< y 523)) #t #f))
  
;-------------------------------CHOOSE COLOUR SCREEN------------------------;
(define (colour-choose-screen)                                              ;SCREEN FOR CHOOSING THE COLOR
  (cond [(equal? 'AI-player (caadr (get-field player-types ludo))) (begin (set! screen 'screen1) (place-image choose-colour-page-1 (car centre-board) (cdr centre-board) scene))]
        [(equal? 2 (length (get-field player-types ludo))) (begin (set! screen 'screen2) (place-image choose-colour-page-2 (car centre-board) (cdr centre-board) scene))]
        [(equal? 3 (length (get-field player-types ludo))) (begin (set! screen 'screen3) (place-image choose-colour-page-3 (car centre-board) (cdr centre-board) scene))]
        [(equal? 4 (length (get-field player-types ludo))) (begin (set! screen 'screen4) (place-image choose-colour-page-4 (car centre-board) (cdr centre-board) scene))]))
(define (choose-color-again)
  (cond  [(equal? 2 (length (get-field player-types ludo))) (begin (place-image choose-colour-page-2 (car centre-board) (cdr centre-board) scene))]
        [(equal? 3 (length (get-field player-types ludo))) (begin (place-image choose-colour-page-3 (car centre-board) (cdr centre-board) scene))]
        [(equal? 4 (length (get-field player-types ludo))) (begin (place-image choose-colour-page-4 (car centre-board) (cdr centre-board) scene))]))
(define (colour-choose sce x y mouse-pos)                                   ;CHANGES THE STATE TO SETUP BOARD AND DICE
  (if (equal? mouse-pos "button-down")
(cond [(and (not(equal? #f (choose x y l1))) (equal? screen 'screen1))  (begin (button-sounds) (colour-setter-AI (choose x y l1) colors)  (send ludo set-coins) (set! state 'draw-it) sce)]
      [(and (not(equal? #f (choose x y l2))) (equal? screen 'screen2)) (begin (button-sounds) (colour-setter-human-2-1 (choose x y l2)) (set! screen 'screen2-final) (set! state 'choose-again) sce)]
      [(and (not(equal? #f (choose x y l3))) (equal? screen 'screen2-final)) (begin (button-sounds) (colour-setter-human-2-2 (choose x y l3)) (send ludo set-coins) (set! state 'draw-it) sce)]
      [(and (not(equal? #f (choose x y l4))) (equal? screen 'screen3)) (begin (button-sounds) (colour-setter-human-3-1 (choose x y l4)) (set! screen 'screen3-1final) (set! state 'choose-again) sce)]
      [(and (not(equal? #f (choose x y l5))) (equal? screen 'screen3-1final)) (begin (button-sounds) (colour-setter-human-3-2 (choose x y l5)) (set! screen 'screen3-final)  (set! state 'choose-again) sce)]
      [(and (not(equal? #f (choose x y l6))) (equal? screen 'screen3-final)) (begin (button-sounds) (colour-setter-human-3-3 (choose x y l6)) (send ludo set-coins) (set! state 'draw-it) sce)]
      [(and (not(equal? #f (choose x y l7))) (equal? screen 'screen4)) (begin (button-sounds) (colour-setter-human-4-1 (choose x y l7)) (set! screen 'screen4-1final) (set! state 'choose-again) sce)]
      [(and (not(equal? #f (choose x y l8))) (equal? screen 'screen4-1final)) (begin (button-sounds) (colour-setter-human-4-2 (choose x y l8)) (set! screen 'screen4-2final) (set! state 'choose-again) sce)]
      [(and (not(equal? #f (choose x y l9))) (equal? screen 'screen4-2final)) (begin (button-sounds) (colour-setter-human-4-3 (choose x y l9)) (set! screen 'screen4-final) (set! state 'choose-again) sce)]
      [(and (not(equal? #f (choose x y l10))) (equal? screen 'screen4-final)) (begin (button-sounds) (colour-setter-human-4-4 (choose x y l10)) (send ludo set-coins) (set! state 'draw-it) sce)]) sce))
(define(choose x y l)                                                         ;REGIONS FOR SELECTING RESPECTIVE COLORS ON THE SCREEN
  (cond [(circle-in (list-ref l 0) (list-ref l 1) (list-ref l 2) x y) "red"]
        [(circle-in (list-ref l 3) (list-ref l 4) (list-ref l 5) x y) "green"]
        [(circle-in (list-ref l 6) (list-ref l 7) (list-ref l 8) x y) "blue"]
        [(circle-in (list-ref l 9) (list-ref l 10) (list-ref l 11) x y) "yellow"]
        [else #f]))
;----------------------------DICE EVENTS-----------------------------;
(define (dice-region x y)                                            ; VALID REGION FOR CLICKING ON THE DICE
  (if (and (<= x (+ (car dice-pos) 25)) (>= x (- (car dice-pos) 25)) (<= y (+ (cdr dice-pos) 25)) (>= y (- (cdr dice-pos) 25))) #t #f))
(define (dice-working sce)  
  (begin (dice-sounds)
    (send dice roll-dice)                ;ROLLING THE DICE
    (let ([index (send dice get-index)])
      (begin (displayln index)
             (cond [(and (not (equal? index 6)) (not (equal? 0 (get-field state-ind dice)))) ;GETTING A NUMBER OTHER THAN 6 AFTER GETTING 6 
                    (begin
                      (send dice refresh)
                      (set! state 'dice-event) sce)]
                
                   [(not (equal? index 6)) (begin ;ANY NUMBER NOT FOLLOWED AFTER 6
                                             (send ludo get-next-player)
                                             (send dice refresh)
                                             (set! state 'dice-event) sce)]

                   [(send dice all-six) (begin  ;3 SIXES IN A ROW 
                                         
                                          (send dice refresh)
                                          (set! state 'draw-it) sce)]
                
                   [(and (equal? index 6) (equal? 0 (get-field state-ind dice))) (begin  ;FIRST 6
                                                                                   (send ludo get-next-player)
                                                                                   (set! state 'dice-event) sce)]
                   [else (begin  ;SECOND 6
                           (set! state 'dice-event) sce)])))))
(define (dice-click sce x y mouse-pos)
  (cond
    [(and (not (equal? (get-field last-index dice) 6))  ;(last index was not 6) and next player is AI
          (equal? 'AI-player (get-field type (send ludo tell-next-player (get-field current-player ludo)))))
     (begin (sleep 1) (dice-working sce))]
    [(and (equal? (get-field last-index dice) 6)  ;(last index was 1st or 2nd 6) and current player is AI
          (equal? 'AI-player (get-field type (send ludo get-current-player))))
     (dice-working sce)]
    [else (if (and (equal? mouse-pos "button-down") (dice-region x y))
              (begin (sleep 1) (dice-working sce))     
              sce)]))

;----------------------COIN MOVING EVENTS----------------------------;
  (define (coin-click sce x y mouse-pos)
  (let ([index (get-field last-index dice)]
        [player (send ludo get-current-player)])
    (cond [(equal? (send player no-move-possible index) #t)
           (begin (set! state 'draw-it) sce)]
          [(equal? (get-field type player) 'AI-player) (begin (send player get-move-AI index)
                                                            (set! state 'draw-it) ;(sleep 1)
                                                            sce)]
          [else (if (equal? mouse-pos "button-down") 
            (let ([get-coin (send player valid-region x y index)])
              (begin
                (displayln get-coin)
                (if (not (equal? get-coin #f))
                    (begin (button-sounds)(send get-coin move index)
                           (set! state 'draw-it) sce)
                    sce)))
            sce)])))

;------------------DRAWING FUNCTION-------------------------------;
(define (drawing sc) 
  (cond [(equal? state 'home) home-screen]
        [(equal? state 'players) players-screen]
        [(equal? state 'type-of-players) player-type-screen]
        [(equal? state 'colour-choose) (colour-choose-screen)]
        [(equal? state 'choose-again) (choose-color-again)]
        [else (place-images (append (list (send dice draw)
                                          (text "MAKE A\nMOVE" 30 "black"))  ;DISPLAYS WHOSE TURN IT IS
                                    (send ludo images)
                                    list-safe-place)
                            (append 
                                    (list (make-posn (car dice-pos) (cdr dice-pos))
                                          (if (equal? (get-field current-player ludo) -1)
                                              (your-turn (get-field color (send ludo tell-next-player (get-field current-player ludo))))
                                           (your-turn (get-field color (send ludo tell-next-player (- (get-field current-player ludo) 1))))))
                                    (send ludo positions)
                                    (map (lambda (x) (make-posn (car x) (cdr x))) safe-place))
                            scene-ludo)]))
;----------------------MOUSE EVENT FUNCTION------------------;
(define (mouse-eve sce x y mouse-pos)
  (cond [(equal? state 'home) (start-click sce x y mouse-pos)]
        [(equal? state 'players) (players sce x y mouse-pos)]
        [(equal? state 'type-of-players) (type-of-players sce x y mouse-pos)]
        [(equal? state 'colour-choose) (colour-choose sce x y mouse-pos)]
        [(equal? state 'choose-again)  (colour-choose sce x y mouse-pos) ]
        [(equal? state 'draw-it) (dice-click sce x y mouse-pos)]
        [(equal? state 'dice-event) (coin-click sce x y mouse-pos)]
        [else sce]))
;-----------------------BIG BANG FUNCTION---------------------;  
 (big-bang emp-scene (to-draw drawing) (on-mouse mouse-eve) (stop-when f game-over))




