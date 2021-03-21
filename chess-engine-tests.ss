(define (get-starting-board-state)
  (fen->state "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"))

(define (get-position-2-state)
  (fen->state "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -"))

(define (get-position-3-state)
  (fen->state "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - -"))

(define (get-position-4-state)
  (fen->state "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1"))

(define (get-position-5-state)
  (fen->state "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8"))

(define (get-position-6-state)
  (fen->state "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10"))

(define (algebraic->indices str)
  (let ([first (string-ref str 0)]
        [second (string-ref str 1)])
    (values
     (char- first #\a)
     (- 8 (- (char->integer second) 48)))))

(define (get-possibilities starting-state starting-depth)
  (let helper ([to-search (list (cons starting-state starting-depth))] [total 0])
    (if (null? to-search)
        total
        (let ([state (caar to-search)]
              [depth (cdar to-search)])
          (if (= 0 depth)
              0
              (let ([color (state-color state)])
                (let ([moves (get-possible-moves state)])
                  (if (= 1 depth)
                      (begin
                        #;(for-each
                         (lambda (move)
                           ;;(display "\033c")
                           ;;(draw-board (state-board (apply-moves-on-new-copy move state)))
                           ;;(sleep (make-time 'time-duration 1000000 0))
                           (cond
                            [(eq? (move-name (car move)) 'en-passant)
                             (set! en-passant-count (1+ en-passant-count))]
                            [(eq? (move-name (car move)) 'castle)
                             (set! castle-count (1+ castle-count))])
                           (let ([to (move-to (car move))])
                             (when (matrix-ref (state-board state) (position-x to) (position-y to))
                               (set! capture-count (1+ capture-count)))))
                         moves)
                        (helper (cdr to-search) (+ total (length moves))))
                      (let ([new-to-search
                             (map
                              (lambda (move)
                                (cons (apply-moves-on-new-copy move state)
                                      (1- depth)))
                              moves)])
                        (helper (append new-to-search (cdr to-search))
                                total))))))))))

(define (fen->state fen)
  (let* ([parts
          (let split-at-spaces ([ls (string->list fen)] [acc '()])
            (cond
             [(null? ls) (list (reverse acc))]
             [(char=? #\ (car ls)) (cons (reverse acc) (split-at-spaces (cdr ls) '()))]
             [else (split-at-spaces (cdr ls) (cons (car ls) acc))]))]
         [fen-board (car parts)]
         [fen-color (cadr parts)]
         [fen-castle-states (caddr parts)]
         [fen-en-passant (cadddr parts)]
         [board (get-empty-board)]
         [x 0]
         [y 0])
    (for-each
     (lambda (p)
       (if (and (< (char->integer p) 57) (not (char=? p #\/)))
           (set! x (+ x (- (char->integer p) 48)))
           (case p
             [#\r (matrix-set! board x y black-rook) (set! x (1+ x))]
             [#\n (matrix-set! board x y black-knight) (set! x (1+ x))]
             [#\b (matrix-set! board x y black-bishop) (set! x (1+ x))]
             [#\k (matrix-set! board x y black-king) (set! x (1+ x))]
             [#\q (matrix-set! board x y black-queen) (set! x (1+ x))]
             [#\p (matrix-set! board x y black-pawn) (set! x (1+ x))]
             [#\R (matrix-set! board x y white-rook) (set! x (1+ x))]
             [#\N (matrix-set! board x y white-knight) (set! x (1+ x))]
             [#\B (matrix-set! board x y white-bishop) (set! x (1+ x))]
             [#\K (matrix-set! board x y white-king) (set! x (1+ x))]
             [#\Q (matrix-set! board x y white-queen) (set! x (1+ x))]
             [#\P (matrix-set! board x y white-pawn) (set! x (1+ x))]
             [#\/ (set! x 0) (set! y (1+ y))])))
     fen-board)
    (let ([color (if (char=? (car fen-color) #\w) 'w 'b)]
          [last-move
           (if (char=? #\- (car fen-en-passant))
               empty-move
               (call-with-values (lambda () (algebraic->indices (list->string fen-en-passant)))
                 (lambda (x y)
                   (make-move (make-position x (if (= y 2) (1- y) (1+ y)))
                              (make-position x (if (= y 2) (1+ y) (1- y)))
                              #f
                              #f
                              0))))]
          [castle-k-w (member #\K fen-castle-states)]
          [castle-q-w (member #\Q fen-castle-states)]
          [castle-k-b (member #\k fen-castle-states)]
          [castle-q-b (member #\q fen-castle-states)])
      (make-state color
                  last-move
                  '()
                  castle-k-w
                  castle-q-w
                  castle-k-b
                  castle-q-b
                  board
                  (get-hash board last-move castle-k-w castle-q-w castle-k-b castle-q-b color)))))

(define (run-tests)
  (for-each
   (lambda (test)
     (let ([name (car test)]
           [state (cadr test)]
           [depth (caddr test)]
           [correct (cadddr test)])
       (let ([actual (get-possibilities state depth)])
         (if (= actual correct)
             (printf "~d at depth ~d: PASS\n" name depth)
             (printf "~d at depth ~d: FAIL: expected ~d, got ~d\n" name depth correct actual)))))
   (list
    (list 'initial-position (get-starting-board-state) 1 20)
    (list 'initial-position (get-starting-board-state) 2 400)
    (list 'initial-position (get-starting-board-state) 3 8902)
    (list 'initial-position (get-starting-board-state) 4 197281)
    
    (list 'position-2 (get-position-2-state) 1 48)
    (list 'position-2 (get-position-2-state) 2 2039)
    (list 'position-2 (get-position-2-state) 3 97862)
    (list 'position-2 (get-position-2-state) 4 4085603)

    (list 'position-3 (get-position-3-state) 1 14)
    (list 'position-3 (get-position-3-state) 2 191)
    (list 'position-3 (get-position-3-state) 3 2812)
    (list 'position-3 (get-position-3-state) 4 43238)

    (list 'position-4 (get-position-4-state) 1 6)
    (list 'position-4 (get-position-4-state) 2 264)
    (list 'position-4 (get-position-4-state) 3 9467)
    (list 'position-4 (get-position-4-state) 4 422333)

    (list 'position-5 (get-position-5-state) 1 44)
    (list 'position-5 (get-position-5-state) 2 1486)
    (list 'position-5 (get-position-5-state) 3 62379)
    (list 'position-5 (get-position-5-state) 4 2103487)

    (list 'position-6 (get-position-6-state) 1 46)
    (list 'position-6 (get-position-6-state) 2 2079)
    (list 'position-6 (get-position-6-state) 3 89890)
    (list 'position-6 (get-position-6-state) 4 3894594))))


;;(time (run-tests))
