#lang racket

(require 2htdp/universe)
(require (prefix-in htdp: 2htdp/image))
(require racket/gui)
(require lang/posn)

(struct player-ranking (points name) #:transparent)

(define (play n player)

    (define n-moves 0)    

    (define size-cell 100)

    (define text-size (* size-cell 0.5))

    (define current-config (build-vector (* n n) add1))

    (define swap-config (build-vector (* n n) add1))
    (vector-set*! swap-config (- (* n n) 1) 0)

    (define final-config (build-vector (* n n) add1))
    (vector-set*! final-config (- (* n n) 1) 0)

    (define end-game-message 
        (htdp:text "GG WP" text-size "red"))

    (define start-menu
        (htdp:text "Entre com o tamanho do jogo: (ex 4)" (* text-size 0.4)"red"))

    (define print-player-name
        (htdp:text player (* text-size 0.4) "green"))

    (define start-layout
        (htdp:empty-scene 400 500))

    (define cell
        (htdp:square size-cell "outline" "black"))

    (define (number-text an-number)
        (cond
            [(= an-number 0) (htdp:text " " text-size "blue")]
            [else (htdp:text (number->string an-number) text-size "blue")]))

    (define (size-of-game)
        (* (- n 1) (- n 1)))

    (define layout
        (htdp:empty-scene (+ (* n size-cell) 100) (* n size-cell) ))

    (define (check-move current-position movement config)
        (and (< (+ current-position movement) (vector-length config))
            (>= (+ current-position movement) 0) (or (= (quotient current-position n) (quotient (+ current-position movement) n)) (= (remainder current-position n) (remainder (+ current-position movement) n))))
        )
    (define (increment!)
        (set! n-moves (add1 n-moves ) )
    )

    (define (move size)
        (lambda (w current-config)
            (define zero-position (vector-member 0 current-config))
            (cond 
            [(check-move zero-position size current-config)
                (let ([swap-value (vector-ref current-config (+ zero-position size))])
                    (vector-set*! current-config zero-position swap-value)
                    (vector-set*! current-config (+ size zero-position) 0)
                    (increment!)
                 ) 
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
            [(key=? key "down") (move-down w current-config)])
    )

    (define (game-over w)
        (equal? current-config final-config)
    )

    (define (draw-scene w)
    (let
        (
        [cells-number (build-list  (* n n) (lambda (x) cell))]
        [current-config-text (build-list (* n n) (lambda (x) (number-text (vector-ref current-config x))))]
        [cells-position (build-list  (* n n) (lambda (x) (make-posn (+ (* (remainder x n) size-cell) (/ size-cell 2)) (+ (* (quotient x n) size-cell) (/ size-cell 2)))))]
        [player-list (list print-player-name)]
        [player-position (list  (make-posn (+ (* n size-cell) 50) (* size-cell 0.5)))]
        [score-list (list (number-text n-moves))]
        [score-position (list (make-posn (+ (* n size-cell) 50) (+ (* size-cell 0.5) 100)))]
        )
    (cond
    [(game-over w) (htdp:place-image end-game-message (/ (* n size-cell) 2) (/ (* n size-cell) 2) layout)]
    [else (htdp:place-images
        (append cells-number current-config-text player-list score-list)
        (append cells-position cells-position player-position score-position)
        layout
        )]
        ))
    )

    (define (draw-menu-scene w)
        (htdp:place-image/align start-menu 200 50 "center" "center" start-layout)
        (cond
            [(game-over w)]
        )
    )

    (define (generate-start-config current-config)
        (for ([i (in-range (* n 300))])
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

    (vector-copy! current-config 0 (generate-start-config swap-config))

    (define (start-game)
        (set! n-moves 0)
        (big-bang 0
            (name "N-PUZZLE")
            (on-key key-function)
            (to-draw draw-scene)
            ;;; (stop-when game-over draw-scene)
        )
        (define out (open-output-file "ranking.txt" #:mode 'text #:exists 'append))
        (write (list (string-append (number->string n-moves) " " player )) out)
        (close-output-port out)
    )

    (define (h1 config)
        (let
            ([resp 0])
            (for ([i  (in-range n*n)])
                (display i)
            )
        )
    )
    (define (a-star)

    )
    (start-game)
    (start-new-game)

)

(define (start-new-game)

    (define (get-ranking pos)
        (let
            ([l1(file->list "ranking.txt")])
            (define l2 (build-list (length l1) (lambda (x) 
                (define aux (string-split (list-ref (list-ref l1 x) 0)) )
                (player-ranking (string->number (list-ref aux 0)) (list-ref aux 1) )
            )))
            (cond
            [(< pos (length l1))
            (string-append (number->string (player-ranking-points (list-ref (sort l2 < #:key player-ranking-points) pos))) " " (player-ranking-name (list-ref (sort l2 < #:key player-ranking-points) pos)))]
            [else (string-append " " " ")]
            
            )
        )
    )

    (define entry-menu (instantiate dialog% ("N-PUZZLE")))
    (define ranking-menu (instantiate dialog% ("RANKING")))
    
    (define n-size (new text-field% [parent entry-menu] [label "Tamanho"]))

    (define player-name (new text-field% [parent entry-menu] [label "Jogador"]))

    (define panel (new horizontal-panel% [parent entry-menu]
                                        [alignment '(center center)]))

    (define ranking-panel (new horizontal-panel% [parent ranking-menu]
                                            [alignment '(center center)]))                                        
    
    (new button% [parent panel] [label "Proximo"]
        [callback (lambda (button event)  (send entry-menu show #f) (play (string->number (send n-size get-value)) (send player-name get-value)))])
    

    (new button% [parent panel] [label "Sair"]
        [callback (lambda (button event)  (send entry-menu show #f) )])

    (new button% [parent panel] [label "Ranking"]
        [callback (lambda (button event)  (send ranking-menu show #t) )])


    (define ranking1 (new message% [parent ranking-panel]
                          [label (string-append "(1). " (get-ranking 0) )]))

    (define ranking2 (new message% [parent ranking-panel]
                          [label (string-append "(2). " (get-ranking 1) )]))

    (define ranking3 (new message% [parent ranking-panel]
                          [label (string-append "(3). " (get-ranking 2) )]))

    (define ranking4 (new message% [parent ranking-panel]
                          [label (string-append "(4). " (get-ranking 3) )]))                                                                            

    (define ranking5 (new message% [parent ranking-panel]
                          [label (string-append "(5). " (get-ranking 4) )]))

    (define ranking6 (new message% [parent ranking-panel]
                          [label (string-append "(6). " (get-ranking 5) )]))

    (define ranking7 (new message% [parent ranking-panel]
                          [label (string-append "(7). " (get-ranking 6) )]))

    (define ranking8 (new message% [parent ranking-panel]
                          [label (string-append "(8). " (get-ranking 7) )]))

    (define ranking9 (new message% [parent ranking-panel]
                          [label (string-append "(9). " (get-ranking 8) )]))          

    (define ranking10 (new message% [parent ranking-panel]
                          [label (string-append "(10). " (get-ranking 9) )]))                                                                                                                                                  


    (send entry-menu show #t)
    
)

(start-new-game)
