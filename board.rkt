#lang racket

(require 2htdp/image 2htdp/universe)
(require lang/posn)

(define n 3)
(define size-cell 100)
(define text-size (* size-cell 0.8))

(define current-config (build-vector (* n n) add1))
(vector-set*! current-config (- (* n n) 1) 0)



(define final-config (build-vector (* n n) add1))
(vector-set*! final-config (- (* n n) 1) 0)

(define cell
    (square size-cell "outline" "black"))

(define (number-text an-number)
    (text (number->string an-number) text-size "blue"))

(define (size-of-game)
    (* (- n 1) (- n 1)))

(define layout
    (empty-scene (* n size-cell) (* n size-cell) ))

(define (check-move current-position movement config)
    (and (< (+ current-position movement) (vector-length config))
         (>= (+ current-position movement) 0) (or (= (quotient current-position n) (quotient (+ current-position movement) n)) (= (remainder current-position n) (remainder (+ current-position movement) n))))
    )
    
(define (move size)
    (lambda (w current-config)
        (define zero-position (vector-member 0 current-config))
        (cond 
        [(check-move zero-position size current-config)
            (let ([swap-value (vector-ref current-config (+ zero-position size))])
                (vector-set*! current-config zero-position swap-value)
                (vector-set*! current-config (+ size zero-position) 0)) 
        ]
        )))

(define move-right (move -1))
(define move-left (move 1))
(define move-up (move n))
(define move-down (move (- n)))

(define (key-function w key)
    (cond
        [(key=? key "right") (move-left w current-config)]
        [(key=? key "left") (move-right w current-config)]
        [(key=? key "up") (move-down w current-config)]
        [(key=? key "down") (move-up w current-config)]))

(define (game-over current-config)
    (equal? current-config final-config))

(define (draw-scene w)
(let
    ([cells-number (build-list  (* n n) (lambda (x) cell))]
    [current-config-text (build-list (* n n) (lambda (x) (number-text (vector-ref current-config x))))]
    [cells-position (build-list  (* n n) (lambda (x) (make-posn (+ (* (remainder x n) size-cell) (/ size-cell 2)) (+ (* (quotient x n) size-cell) (/ size-cell 2)))))])
  (place-images
    (append cells-number current-config-text)
    (append cells-position cells-position)
    layout
    ))
  
)

(big-bang 0
    (on-key key-function)
    (to-draw draw-scene)
)