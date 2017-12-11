#lang racket

(require 2htdp/image 2htdp/universe)
(require lang/posn)

(define n 2)
(define size-cell 100)
(define text-size (* size-cell 0.5))

(define current-config (build-vector (* n n) add1))

(define swap-config (build-vector (* n n) add1))
(vector-set*! swap-config (- (* n n) 1) 0)

(define final-config (build-vector (* n n) add1))
(vector-set*! final-config (- (* n n) 1) 0)

(define end-game-message 
    (text "GG WP" text-size "red"))

(define start-menu
    (text "Entre com o tamanho do jogo: (ex 4)" (* text-size 0.4)"red"))

(define start-layout
    (empty-scene 400 500))

(define cell
    (square size-cell "outline" "black"))

(define (number-text an-number)
    (cond
        [(= an-number 0) (text " " text-size "blue")]
        [else (text (number->string an-number) text-size "blue")]))

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
        [(key=? key "right") (move-right w current-config)]
        [(key=? key "left") (move-left w current-config)]
        [(key=? key "up") (move-up w current-config)]
        [(key=? key "down") (move-down w current-config)]))

(define (game-over w)
    (equal? current-config final-config)
)

(define (draw-scene w)
(let
    ([cells-number (build-list  (* n n) (lambda (x) cell))]
    [current-config-text (build-list (* n n) (lambda (x) (number-text (vector-ref current-config x))))]
    [cells-position (build-list  (* n n) (lambda (x) (make-posn (+ (* (remainder x n) size-cell) (/ size-cell 2)) (+ (* (quotient x n) size-cell) (/ size-cell 2)))))])
  (cond
  [(game-over w) (place-image end-game-message (/ (* n size-cell) 2) (/ (* n size-cell) 2) layout)]
  [else (place-images
    (append cells-number current-config-text)
    (append cells-position cells-position)
    layout
    )]
    ))
)

(define (draw-menu-scene w)
    (place-image/align start-menu 200 50 "center" "center" start-layout)
)

(define (generate-start-config current-config)
    (for ([i (in-range 1000)])
            (let 
                ([x (random 4)])
                (cond 
                    [(= x 0) (move-right 0 current-config)]
                    [(= x 1) (move-left 0 current-config)]
                    [(= x 2) (move-up 0 current-config)]
                    [(= x 3) (move-down 0 current-config)]
                )
            )
    )
    current-config
)

;;; (define (get-entry w key)
;;;     (cond
;;;         [(key=? key "right") (move-right w current-config)]
;;;         [(key=? key "left") (move-left w current-config)]
;;;         [(key=? key "up") (move-up w current-config)]
;;;         [(key=? key "down") (move-down w current-config)])
;;;         [(number? key) ()]        
;;; )

(vector-copy! current-config 0 (generate-start-config swap-config))
current-config

;;; (big-bang 0
;;;     (to-draw draw-menu-scene)
;;;     (on-key get-entry)
;;; )

(big-bang 0 
    (name "N-PUZZLE")
    (on-key key-function)
    (to-draw draw-scene)
    (stop-when game-over draw-scene)
)
