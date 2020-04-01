#lang racket
(require lang/posn)
(provide (all-defined-out))
(define dice-pos (cons 70 70)) ;POSITION OF THE DICE ON THE SCREEN
(define add-constant 115)
(define board-dimension 690)
(define a (/ board-dimension 15))
(define t (/ a 2))
(define big-square (* a 6))     
(define home-square (* a 3))
(define home-dimension big-square)
(define colors (list "red" "green" "blue" "yellow"))
(define colors-s (list "red" "blue" "yellow" "green"))
(define centre-board (cons 460 345))
(define (your-turn color)
  (cond [(equal? color "red") (make-posn 56 150)]
        [(equal? color "yellow") (make-posn 863 495)]
        [(equal? color "blue") (make-posn 863 150)]
        [(equal? color "green") (make-posn 56 495)]))

(define safe-place                             ;SAFE-PLACE COORDINATES
  (list
   (cons (+ add-constant (* 3 t)) (+ big-square t))
   (cons (+ add-constant (* 5 t)) (+ big-square (* 5 t)))
   (cons (+ add-constant (+ big-square t)) (* 5 t))
   (cons (+ add-constant (+ big-square (* 5 t))) (* 3 t))
   (cons (+ add-constant (+ big-square t)) (- board-dimension (* 3 t)))
   (cons (+ add-constant (+ big-square (* 5 t))) (- board-dimension (* 5 t)))
   (cons (+ add-constant (- board-dimension (* 5 t))) (+ big-square t))
   (cons (+ add-constant (- board-dimension (* 3 t))) (+ big-square (* 5 t)))))
(define l1 '(213 461 70 393 464 77 570 470 77 741 475 70))   ;COORDINATES FOR DIFFERENT SELECTING REGIONS
(define l2 '(444 291 48 555 286 47 667 287 45 775 284 50))
(define l3 '(438 486 52 556 483 51 674 479 52 784 477 50))
(define l4 '(479 254 40 578 257 41 680 253 43 784 260 46))
(define l5 '(482 380 40 583 387 41 683 386 47 787 382 45))
(define l6 '(481 498 45 583 495 46 690 496 44 795 495 42))
(define l7 '(440 220 40 535 218 37 637 225 44 739 216 42))
(define l8 '(446 336 43 542 336 44 644 333 45 748 334 47))
(define l9 '(446 444 42 539 452 43 642 449 42 740 456 44))
(define l10 '(451 559 42 543 564 42 639 558 48 747 558 46))

(define start-pos                                             ;START POSITION COORDINATES
  (make-hash
   (list
   (cons "red" (cons (+ add-constant (* 3 t)) (* 13 t)))
   (cons "green" (cons (+ add-constant (* 13 t)) (* 27 t)))
   (cons "yellow" (cons (+ add-constant (* 27 t)) (* 17 t)))
   (cons "blue" (cons (+ add-constant (* 17 t)) (* 3 t))))))
(define (square-region x y centre-x centre-y)                 ;INTERIOR OF A SQUARE
  (if (and (<= x (+ centre-x t)) (>= x (- centre-x t)) (<= y (+ centre-y t)) (>= y (- centre-y t))) #t #f))
    
     