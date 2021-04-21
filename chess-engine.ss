(begin
  (define seed (time-nanosecond (current-time)));; 335091634)
  (random-seed seed))

(define counter 0)
(define hash-counter 0)

(define-record-type position
  (fields x y))

(define-record-type move
  (fields from to promotion name value))

(define empty-move (make-move (make-position 0 0) (make-position 0 0) #f #f 0))

(define move-to-attacked 10)
(define move-to-not-attacked 10)
(define capture-at-attacked 50)
(define capture-at-not-attacked 90)

(define-record-type state
  (fields color last-move moves castle-k-w castle-q-w castle-k-b castle-q-b board hash))

(define white-king #b0000)
(define white-queen #b0001)
(define white-rook #b0010)
(define white-bishop #b0011)
(define white-knight #b0100)
(define white-pawn #b0101)
(define black-king #b1000)
(define black-queen #b1001)
(define black-rook #b1010)
(define black-bishop #b1011)
(define black-knight #b1100)
(define black-pawn #b1101)

(define pawn-table
  '#(#(0 0 0 0 0 0 0 0)
     #(50 50 50 50 50 50 50 50)
     #(10 10 20 30 30 20 10 10)
     #( 5 5 10 25 25 10 5 5)
     #( 0 0 0 20 20 0 0 0)
     #( 5 -5 -10 0 0 -10 -5 5)
     #( 5 10 10 -20 -20 10 10 5)
     #( 0 0 0 0 0 0 0 0)))

(define knight-table
  '#(#(-50 -40 -30 -30 -30 -30 -40 -50)
     #(-40 -20 0 0 0 0 -20 -40)
     #(-30 0 10 15 15 10 0 -30)
     #(-30 5 15 20 20 15 5 -30)
     #(-30 0 15 20 20 15 0 -30)
     #(-30 5 10 15 15 10 5 -30)
     #(-40 -20 0 5 5 0 -20 -40)
     #(-50 -40 -30 -30 -30 -30 -40 -50)))

(define bishop-table
  '#(#(-20 -10 -10 -10 -10 -10 -10 -20)
     #(-10 0 0 0 0 0 0 -10)
     #(-10 0 5 10 10 5 0 -10)
     #(-10 5 5 10 10 5 5 -10)
     #(-10 0 10 10 10 10 0 -10)
     #(-10 10 10 10 10 10 10 -10)
     #(-10 5 0 0 0 0 5 -10)
     #(-20 -10 -10 -10 -10 -10 -10 -20)))

(define rook-table
  '#(#(0 0 0 0 0 0 0 0)
     #(5 10 10 10 10 10 10 5)
     #(-5 0 0 0 0 0 0 -5)
     #(-5 0 0 0 0 0 0 -5)
     #(-5 0 0 0 0 0 0 -5)
     #(-5 0 0 0 0 0 0 -5)
     #(-5 0 0 0 0 0 0 -5)
     #(0 0 0 5 5 0 0 0)))

(define queen-table
  '#(#(-20 -10 -10 -5 -5 -10 -10 -20)
     #(-10 0 0 0 0 0 0 -10)
     #(-10 0 5 5 5 5 0 -10)
     #(-5 0 5 5 5 5 0 -5)
     #(0 0 5 5 5 5 0 -5)
     #(-10 5 5 5 5 5 0 -10)
     #(-10 0 5 0 0 0 0 -10)
     #(-20 -10 -10 -5 -5 -10 -10 -20)))

(define king-middle-table
  '#(#(-30 -40 -40 -50 -50 -40 -40 -30)
     #(-30 -40 -40 -50 -50 -40 -40 -30)
     #(-30 -40 -40 -50 -50 -40 -40 -30)
     #(-30 -40 -40 -50 -50 -40 -40 -30)
     #(-20 -30 -30 -40 -40 -30 -30 -20)
     #(-10 -20 -20 -20 -20 -20 -20 -10)
     #( 20 20 0 0 0 0 20 20)
     #( 20 30 10 0 0 10 30 20)))

(define king-end-table
  '#(#(-50 -40 -30 -20 -20 -30 -40 -50)
     #(-30 -20 -10 0 0 -10 -20 -30)
     #(-30 -10 20 30 30 20 -10 -30)
     #(-30 -10 30 40 40 30 -10 -30)
     #(-30 -10 30 40 40 30 -10 -30)
     #(-30 -10 20 30 30 20 -10 -30)
     #(-30 -30 0 0 0 0 -30 -30)
     #(-50 -30 -30 -30 -30 -30 -30 -50)))

(define (get-empty-board)
  (vector (make-vector 8 #f)
          (make-vector 8 #f)
          (make-vector 8 #f)
          (make-vector 8 #f)
          (make-vector 8 #f)
          (make-vector 8 #f)
          (make-vector 8 #f)
          (make-vector 8 #f)))

(define matrix-ref
  (lambda (m i j)
    (vector-ref (vector-ref m j) i)))

(define matrix-set!
  (lambda (m i j x)
    (vector-set! (vector-ref m j) i x)))

(load "/home/luke/chess/chess-engine-tests.ss")
(load "/home/luke/chess/zobrist.ss")

(define (draw-board board)
  (newline)
  (vector-for-each
   (lambda (row x)
     (vector-for-each
      (lambda (p y)
        (let ([char
               (cond
                [(not p) #\ ]
                [(= p white-king) #\x2654]
                [(= p white-queen) #\x2655]
                [(= p white-rook) #\x2656]
                [(= p white-bishop) #\x2657]
                [(= p white-knight) #\x2658]
                [(= p white-pawn) #\x2659]
                [(= p black-king) #\x265A]
                [(= p black-queen) #\x265B]
                [(= p black-rook) #\x265C]
                [(= p black-bishop) #\x265D]
                [(= p black-knight) #\x265E]
                [(= p black-pawn) #\x265F]
                [else #\ ])])
          (if (= 0 (mod (+ x y) 2))
              (display (string-append (string char) " "))
              (display (string-append
                        "\033[47m"
                        (string char)
                        " \033[0m")))))
      row
      (list->vector (iota (vector-length board))))
     (newline))
   board
   (list->vector (iota (vector-length board)))))

(define (black? p)
  (and p
       (bitwise-bit-set? p 3)))
(define (white? p)
  (and p
       (not (black? p))))
(define (is-color? p color)
  ((if (eq? color 'w) white? black?) p))
(define (other-color color)
  (if (eq? color 'w) 'b 'w))

(define (king? p)
  (or (= p 0) (= p 8)))
(define (queen? p)
  (or (= p 1) (= p 9)))
(define (rook? p)
  (or (= p 2) (= p 10)))
(define (bishop? p)
  (or (= p 3) (= p 11)))
(define (knight? p)
  (or (= p 4) (= p 12)))
(define (pawn? p)
  (or (= p 5) (= p 13)))

(define (get-possible-moves state)
  (let ([board (state-board state)]
        [color (state-color state)]
        [castle-k-w (state-castle-k-w state)]
        [castle-q-w (state-castle-q-w state)]
        [castle-k-b (state-castle-k-b state)]
        [castle-q-b (state-castle-q-b state)]
        [last-move (state-last-move state)])
    (filter
     (lambda (move)
       (not (in-check? color (state-board (apply-moves-on-new-copy move state)))))
     (apply append
            (apply append
                   (map
                    (lambda (row y)
                      (map
                       (lambda (p x)
                         (let ([check? (if (eq? color 'w) white? black?)])
                           (if (and p (check? p))
                               (cond
                                [(king? p)
                                 (let ([castle-k (if (eq? color 'w) castle-k-w castle-k-b)]
                                       [castle-q (if (eq? color 'w) castle-q-w castle-q-b)])
                                   (get-king-moves x y castle-k castle-q color board))]
                                [(queen? p) (get-sliding-piece-moves x y color board
                                                                     '((1 0) (0 1) (-1 0) (0 -1)
                                                                       (1 1) (1 -1) (-1 1) (-1 -1)))]
                                [(rook? p) (get-sliding-piece-moves x y color board
                                                                    '((1 0) (0 1) (-1 0) (0 -1)))]
                                [(bishop? p) (get-sliding-piece-moves x y color board
                                                                      '((1 1) (1 -1) (-1 1) (-1 -1)))]
                                [(knight? p) (get-knight-moves x y color board)]
                                [(pawn? p) (get-pawn-moves x y last-move color board)])
                               '())))
                       (vector->list row)
                       (iota (vector-length row))))
                    (vector->list board)
                    (iota (vector-length board))))))))

(define (square-on-board? x y)
  (and (<= x 7)
       (>= x 0)
       (<= y 7)
       (>= y 0)))

(define (square-empty-and-on-board? x y board)
  (and (<= x 7)
       (>= x 0)
       (<= y 7)
       (>= y 0)
       (not (matrix-ref board x y))))

(define (square-nonempty-and-on-board? x y board)
  (and (<= x 7)
       (>= x 0)
       (<= y 7)
       (>= y 0)
       (matrix-ref board x y)))

(define (square-empty-or-capturable-and-on-board? x y color board)
  (and (<= x 7)
       (>= x 0)
       (<= y 7)
       (>= y 0)
       (let ([p (matrix-ref board x y)])
         (or (not p)
             (not (is-color? p color))))))

(define (square-capturable-and-on-board? x y color board)
  (and (<= x 7)
       (>= x 0)
       (<= y 7)
       (>= y 0)
       (let ([p (matrix-ref board x y)])
         (and p
              (not (is-color? p color))))))

(define (evaluate-whether-attacked x y color board attacked-value free-value)
  (if (square-under-attack? x y color board)
      attacked-value
      free-value))

(define (get-sliding-piece-moves x y color board directions)
  (let helper ([directions directions]
               [moving-x x]
               [moving-y y])
    (if (null? directions)
        '()
        (let* ([d (car directions)]
               [new-x (+ moving-x (car d))]
               [new-y (+ moving-y (cadr d))])
          (if (square-on-board? new-x new-y)
              (cond
               [(square-empty-and-on-board? new-x new-y board)
                (cons (list (make-move (make-position x y)
                                       (make-position new-x new-y)
                                       #f
                                       'move
                                       (evaluate-whether-attacked new-x new-y color board move-to-attacked move-to-not-attacked)))
                      (helper directions new-x new-y))]
               [(is-color? (matrix-ref board new-x new-y) color)
                (helper (cdr directions) x y)]
               [else ;; other color
                (cons (list (make-move (make-position x y)
                                       (make-position new-x new-y)
                                       #f
                                       'capture
                                       (evaluate-whether-attacked new-x new-y color board capture-at-attacked capture-at-not-attacked)))
                      (helper (cdr directions) x y))])
              (helper (cdr directions) x y))))))

(define (get-knight-moves x y color board)
  (let helper ([possibilities '((1 2) (2 1) (1 -2) (2 -1) (-1 2) (-2 1) (-1 -2) (-2 -1))])
    (if (null? possibilities)
        '()
        (let ([new-x (+ x (caar possibilities))]
              [new-y (+ y (cadar possibilities))])
          (if (square-empty-or-capturable-and-on-board? new-x new-y color board)
              (cons
               (let ([p (matrix-ref board new-x new-y)])
                 (if (and p (is-color? p (other-color color)))
                     (list (make-move (make-position x y) (make-position new-x new-y) #f 'capture
                                      (evaluate-whether-attacked new-x new-y color board capture-at-attacked capture-at-not-attacked)))
                     (list (make-move (make-position x y) (make-position new-x new-y) #f 'move
                                      (evaluate-whether-attacked new-x new-y color board move-to-attacked move-to-not-attacked)))))
               (helper (cdr possibilities)))
              (helper (cdr possibilities)))))))

(define (get-king-moves x y castle-k castle-q color board)
  (append
   (let helper ([possibilities '((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1))])
     (if (null? possibilities)
         '()
         (let ([new-x (+ x (caar possibilities))]
               [new-y (+ y (cadar possibilities))])
           (if (square-empty-or-capturable-and-on-board? new-x new-y color board)
               (cons
                (let ([p (matrix-ref board new-x new-y)])
                  (if (and p (is-color? p (other-color color)))
                      (list (make-move (make-position x y) (make-position new-x new-y) #f 'capture
                                       (evaluate-whether-attacked new-x new-y color board capture-at-attacked capture-at-not-attacked)))
                      (list (make-move (make-position x y) (make-position new-x new-y) #f 'move
                                       (evaluate-whether-attacked new-x new-y color board move-to-attacked move-to-not-attacked)))))
                (helper (cdr possibilities)))
               (helper (cdr possibilities))))))
   (let get-castle-k ()
     (if (and
          castle-k
          (not (in-check? color board))
          (square-empty-and-on-board? (1+ x) y board)
          (not (square-under-attack? (1+ x) y color board))
          (square-empty-and-on-board? (+ 2 x) y board)
          (not (square-under-attack? (+ 2 x) y color board)))
         (list (list (make-move (make-position x y) (make-position (+ 2 x) y) #f 'castle 70)
                     (make-move (make-position (+ 3 x) y) (make-position (+ 1 x) y) #f 'castle 70)))
         '()))
   (let get-castle-q ()
     (if (and
          castle-q
          (not (in-check? color board))
          (square-empty-and-on-board? (1- x) y board)
          (not (square-under-attack? (1- x) y color board))
          (square-empty-and-on-board? (- x 2) y board)
          (not (square-under-attack? (- x 2) y color board))
          (square-empty-and-on-board? (- x 3) y board))
         (list (list (make-move (make-position x y) (make-position (- x 2) y) #f 'castle 70)
                     (make-move (make-position (- 4 x) y) (make-position (1- x) y) #f 'castle 70)))
         '()))))

(define (get-promotion-list from-x from-y to-x to-y color board)
  (map
   (lambda (p)
     (list (make-move (make-position from-x from-y)
                      (make-position to-x to-y)
                      (+ (if (eq? color 'w) 0 8)
                         p)
                      'promotion
                      (evaluate-whether-attacked to-x to-y color board 70 (if (queen? p) 100 50)))))
   (list white-queen white-rook white-knight white-bishop)))

(define (get-pawn-moves x y last-move color board)
  (let ([next-square-proc (if (eq? color 'w) 1- 1+)]
        [starting-square (if (eq? color 'w) 6 1)]
        [en-passant-square (if (eq? color 'w) 3 4)]
        [promotion-square (if (eq? color 'w) 0 7)]
        [last-move-to-x (position-x (move-to last-move))]
        [last-move-to-y (position-y (move-to last-move))]
        [last-move-from-x (position-x (move-from last-move))]
        [last-move-from-y (position-y (move-from last-move))])
    (append
     (if (square-empty-and-on-board? x (next-square-proc y) board)
         (if (= (next-square-proc y) promotion-square)
             (get-promotion-list x y x (next-square-proc y) color board)
             (list (list (make-move (make-position x y)
                                    (make-position x (next-square-proc y))
                                    #f
                                    'move
                                    (evaluate-whether-attacked x (next-square-proc y) color board move-to-attacked move-to-not-attacked)))))
         '())
     (if (and (= y starting-square)
              (square-empty-and-on-board? x (next-square-proc (next-square-proc y)) board)
              (square-empty-and-on-board? x (next-square-proc y) board))
         (list (list (make-move (make-position x y)
                                (make-position x (next-square-proc (next-square-proc y)))
                                #f
                                'move
                                (evaluate-whether-attacked x (next-square-proc (next-square-proc y))
                                                           color board move-to-attacked move-to-not-attacked))))
         '())
     (if (square-capturable-and-on-board? (1+ x) (next-square-proc y) color board)
         (if (= (next-square-proc y) promotion-square)
             (get-promotion-list x y (1+ x) (next-square-proc y) color board)
             (list (list (make-move (make-position x y)
                                    (make-position (1+ x) (next-square-proc y))
                                    #f
                                    'capture
                                    (evaluate-whether-attacked (1+ x) (next-square-proc y) color board capture-at-attacked capture-at-not-attacked)))))
         '())
     (if (square-capturable-and-on-board? (1- x) (next-square-proc y) color board)
         (if (= (next-square-proc y) promotion-square)
             (get-promotion-list x y (1- x) (next-square-proc y) color board)
             (list (list (make-move (make-position x y)
                                    (make-position (1- x) (next-square-proc y))
                                    #f
                                    'capture
                                    (evaluate-whether-attacked (1- x) (next-square-proc y) color board capture-at-attacked capture-at-not-attacked)))))
         '())
     (if (and (= y en-passant-square)
              (= last-move-to-x (1+ x))
              (= last-move-to-y y)
              (= last-move-from-y (next-square-proc (next-square-proc y)))
              (square-nonempty-and-on-board? (1+ x) y board)
              (pawn? (matrix-ref board (1+ x) y))
              (not (is-color? (matrix-ref board (1+ x) y) color)))
         (list (list
                (make-move (make-position (1+ x) (next-square-proc y))
                           (make-position (1+ x) y)
                           #f
                           'en-passant
                           (evaluate-whether-attacked (1+ x) (next-square-proc y) color board capture-at-attacked capture-at-not-attacked))
                (make-move (make-position x y)
                           (make-position (1+ x) (next-square-proc y))
                           #f
                           'en-passant
                           (evaluate-whether-attacked (1+ x) (next-square-proc y) color board capture-at-attacked capture-at-not-attacked))))
         '())
     (if (and (= y en-passant-square)
              (= last-move-to-x (1- x))
              (= last-move-to-y y)
              (= last-move-from-y (next-square-proc (next-square-proc y)))
              (square-nonempty-and-on-board? (1- x) y board)
              (pawn? (matrix-ref board (1- x) y))
              (not (is-color? (matrix-ref board (1- x) y) color)))
         (list (list
                (make-move (make-position (1- x) (next-square-proc y))
                           (make-position (1- x) y)
                           #f
                           'en-passant
                           (evaluate-whether-attacked (1- x) (next-square-proc y) color board capture-at-attacked capture-at-not-attacked))
                (make-move (make-position x y)
                           (make-position (1- x) (next-square-proc y))
                           #f
                           'en-passant
                           (evaluate-whether-attacked (1- x) (next-square-proc y) color board capture-at-attacked capture-at-not-attacked))))
         '()))))

(define (get-changed-castle-info move)
  (let* ([from (move-from move)]
         [to (move-to move)]
         [from-x (position-x from)]
         [from-y (position-y from)]
         [to-x (position-x to)]
         [to-y (position-y to)])
    (let ([castle-k-w
           (not (or (and (= from-x 4) (and (= from-y 7)))
                    (and (= from-x 7) (and (= from-y 7)))
                    (and (= to-x 7) (and (= to-y 7)))))]
          [castle-q-w
           (not (or (and (= from-x 4) (and (= from-y 7)))
                    (and (= from-x 0) (and (= from-y 7)))
                    (and (= to-x 0) (and (= to-y 7)))))]
          [castle-k-b
           (not (or (and (= from-x 4) (and (= from-y 0)))
                    (and (= from-x 7) (and (= from-y 0)))
                    (and (= to-x 7) (and (= to-y 0)))))]
          [castle-q-b
           (not (or (and (= from-x 4) (and (= from-y 0)))
                    (and (= from-x 0) (and (= from-y 0)))
                    (and (= to-x 0) (and (= to-y 0)))))])
      (values castle-k-w castle-q-w castle-k-b castle-q-b))))

(define (apply-moves original-moves state)
  (let ([new-hash (apply-moves-to-hash original-moves (state-board state) (state-hash state))])
    (call-with-values (lambda () (get-changed-castle-info (car original-moves)))
      (lambda (k-w q-w k-b q-b)
        (let helper ([moves original-moves]
                     [last-move #f])
          (let ([board (state-board state)]
                [color (state-color state)]
                [castle-k-w (state-castle-k-w state)]
                [castle-q-w (state-castle-q-w state)]
                [castle-k-b (state-castle-k-b state)]
                [castle-q-b (state-castle-q-b state)]
                [hash (state-hash state)])
            (if (null? moves)
                (make-state (other-color color)
                            last-move
                            (cons last-move (state-moves state))
                            (if castle-k-w k-w #f)
                            (if castle-q-w q-w #f)
                            (if castle-k-b k-b #f)
                            (if castle-q-b q-b #f)
                            board
                            new-hash)
                (let ([to (move-to (car moves))]
                      [from (move-from (car moves))]
                      [promotion (move-promotion (car moves))])
                  (matrix-set! board
                               (position-x to)
                               (position-y to)
                               (if promotion
                                   promotion
                                   (matrix-ref board
                                               (position-x from)
                                               (position-y from))))
                  (matrix-set! board
                               (position-x from)
                               (position-y from)
                               #f)
                  (helper (cdr moves) (car moves))))))))))

(define (copy-board board)
  (let ([new-board (make-vector 8 #f)])
    (do ((x 0 (1+ x))) ((= x 8))
      (vector-set! new-board x (make-vector 8 #f))
      (do ((y 0 (1+ y))) ((= y 8))
        (matrix-set! new-board y x (matrix-ref board y x))))
    new-board))

(define (apply-moves-on-new-copy moves state)
  (let* ([board (state-board state)]
         [new-board (copy-board board)]
         [new-state (make-state (state-color state) (state-last-move state) (state-moves state)
                                (state-castle-k-w state) (state-castle-q-w state)
                                (state-castle-k-b state) (state-castle-q-b state) new-board
                                (state-hash state))])
    (apply-moves moves new-state)))

(define (insufficient-material? state)
  (let ([board (state-board state)])
    (= (apply
        +
        (apply
         append (map
                 (lambda (row)
                   (map
                    (lambda (p) (if p 1 0))
                    (vector->list row)))
                 (vector->list board))))
       2)))

(define (play-one-game state p1 p2)
  (let helper ([state state] [move-count 0])
    (if (insufficient-material? state)
        0
        (let* ([board (state-board state)]
               [color (state-color state)]
               [moves (get-possible-moves state)])
          (if (null? moves)
              (if (in-check? color board)
                  (if (equal? color 'w) -1 1)
                  0)
              (let ([selected-move ((if (eq? color 'w) p1 p2) moves state)])
                (printf "move ~d\n" selected-move)
                (let ([new-state (apply-moves selected-move state)])
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  ;;(display "\033c")
                  (draw-board board)
                  (printf "possible moves: ~d\n" (length moves))
                  (printf "move number: ~d\n" move-count)
                  (printf "seed: ~d\n" seed)
                  (sleep (make-time 'time-duration 100000000 0))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  (helper new-state (1+ move-count)))))))))

(define (get-king-position color board)
  (let ([king (if (eq? color 'w) 0 8)])
    (call/cc
     (lambda (k)
       (do ((x 0 (1+ x))) ((= x 8))
         (do ((y 0 (1+ y))) ((= y 8))
           (let ([p (matrix-ref board x y)])
             (if (and p (= p king))
                 (k (make-position x y))))))
       #f))))

(define (square-under-attack? x y color board)
  (let* ([offset (if (eq? color 'w) 8 0)]
         [king (+ offset white-king)]
         [queen (+ offset white-queen)]
         [rook (+ offset white-rook)]
         [bishop (+ offset white-bishop)]
         [knight (+ offset white-knight)]
         [pawn (+ offset white-pawn)]
         [op-color (other-color color)]
         [pawn-direction (if (eq? color 'w) -1 1)])
    (or
     (and (square-on-board? (1+ x) (+ pawn-direction y))
          (let ([p (matrix-ref board (1+ x) (+ pawn-direction y))])
            (and p
                 (= pawn p))))
     (and (square-on-board? (1- x) (+ pawn-direction y))
          (let ([p (matrix-ref board (1- x) (+ pawn-direction y))])
            (and p
                 (= pawn p))))
     (let check-knights
         ([spots '((1 2) (2 1) (1 -2) (2 -1) (-1 2) (-2 1) (-1 -2) (-2 -1))])
       (if (null? spots)
           #f
           (let ([spot (car spots)])
             (or (check-knights (cdr spots))
                 (and (square-on-board? (+ x (car spot)) (+ y (cadr spot)))
                      (let ([p (matrix-ref board (+ x (car spot))
                                           (+ y (cadr spot)))])
                        (and (is-color? p op-color)
                             (knight? p))))))))
     (let check-lines ([moving-x x]
                       [moving-y y]
                       [directions '((1 0) (0 1) (-1 0) (0 -1))])
       (if (null? directions)
           #f
           (let* ([d (car directions)]
                  [new-x (+ (car d) moving-x)]
                  [new-y (+ (cadr d) moving-y)])
             (cond
              [(not (square-on-board? new-x new-y))
               (check-lines x y (cdr directions))]
              [(square-empty-and-on-board? new-x new-y board)
               (check-lines new-x new-y directions)]
              [(is-color? (matrix-ref board new-x new-y) color)
               (check-lines x y (cdr directions))]
              [else
               (let ([p (matrix-ref board new-x new-y)])
                 (or (= p queen)
                     (= p rook)
                     (check-lines x y (cdr directions))))]))))
     (let check-diagonals ([moving-x x]
                           [moving-y y]
                           [directions '((1 1) (-1 1) (-1 -1) (1 -1))])
       (if (null? directions)
           #f
           (let* ([d (car directions)]
                  [new-x (+ (car d) moving-x)]
                  [new-y (+ (cadr d) moving-y)])
             (cond
              [(not (square-on-board? new-x new-y))
               (check-diagonals x y (cdr directions))]
              [(square-empty-and-on-board? new-x new-y board)
               (check-diagonals new-x new-y directions)]
              [(is-color? (matrix-ref board new-x new-y) color)
               (check-diagonals x y (cdr directions))]
              [else
               (let ([p (matrix-ref board new-x new-y)])
                 (or (= p queen)
                     (= p bishop)
                     (check-diagonals x y (cdr directions))))]))))
     (let check-king ([spots '((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1))])
       (if (null? spots)
           #f
           (let ([spot (car spots)])
             (or
              (check-king (cdr spots))
              (let ([new-x (+ x (car spot))]
                    [new-y (+ y (cadr spot))])
                (and (square-on-board? new-x new-y)
                     (let ([p (matrix-ref board new-x new-y)])
                       (and p
                            (= king p))))))))))))

(define (in-check? color board)
  (let ([king-position (get-king-position color board)])
    (if (not king-position)
        (printf "there is no king seed: ~d\n" seed)
        (let ([king-x (position-x king-position)]
              [king-y (position-y king-position)])
          (square-under-attack? king-x king-y color board)))))

(define (in-checkmate? color state)
  (and (in-check? color (state-board state))
       (null? (get-possible-moves state))))

(define (move-inverse? m1 m2)
  (let* ([f1 m1]
         [f2 m2]
         [from1 (move-from f1)]
         [to1 (move-to f1)]
         [from2 (move-from f2)]
         [to2 (move-to f2)])
    (and (= (position-x from1) (position-x to2))
         (= (position-y from1) (position-y to2))
         (= (position-x to1) (position-x from2))
         (= (position-y to1) (position-y from2)))))

(define (is-repetition? moves)
  (and
   (not (null? moves))
   (not (null? (cdr moves)))
   (not (null? (cddr moves)))
   (not (null? (cdddr moves)))
   (not (null? (cddddr moves)))
   (move-inverse? (car moves) (caddr moves))
   (move-inverse? (cadr moves) (cadddr moves))))

(define (evaluate-material state depth evaluation-hashtable)
  (set! hash-counter (1+ hash-counter))
  (let* ([hash (state-hash state)]
         [previous (hashtable-ref evaluation-hashtable hash #f)])
    ;;(printf "evaluating board with hash ~d\n" hash)
    (if previous
        (begin
          ;;(draw-board (state-board state))
          ;;(printf "hash ~d previous: ~d\n" hash (inexact previous))
          previous)
        (begin
          (set! counter (1+ counter))
          (let ([board (state-board state)]
                [color (state-color state)]
                [moves (state-moves state)]
                [score 0]
                [king-w-x #f]
                [king-w-y #f]
                [king-b-x #f]
                [king-b-y #f]
                [major-piece-value 0])
            (if (is-repetition? moves)
                0
                (cond
                 [(in-checkmate? color state) (if (eq? color 'b) (+ 10000 depth) (- -10000 depth))]
                 #;[(in-stalemate? color state) (if (eq? color 'b) -inf.0 +inf.0)]
                 [else
                  (begin
                    (do ((x 0 (1+ x))) ((= x 8))
                      (do ((y 0 (1+ y))) ((= y 8))
                        (let ([p (matrix-ref board x y)])
                          (when p
                            (let ([table-x (if (white? p) x (- 7 x))]
                                  [table-y (if (white? p) y (- 7 y))])
                              (if (king? p)
                                  (if (white? p)
                                      (begin
                                        (set! king-w-x x)
                                        (set! king-w-y y))
                                      (begin
                                        (set! king-b-x x)
                                        (set! king-b-y y)))
                                  (set! score
                                        (+ score
                                           ((if (white? p) + -)
                                            (cond
                                             [(king? p) 20000]
                                             [(queen? p)
                                              (set! major-piece-value (+ 4 major-piece-value))
                                              (+ 900 (matrix-ref queen-table table-x table-y))]
                                             [(rook? p)
                                              (set! major-piece-value (+ 2 major-piece-value))
                                              (+ 500 (matrix-ref rook-table table-x table-y))]
                                             [(bishop? p)
                                              (set! major-piece-value (+ 1 major-piece-value))
                                              (+ 330 (matrix-ref bishop-table table-x table-y))]
                                             [(knight? p)
                                              (set! major-piece-value (+ 1 major-piece-value))
                                              (+ 320 (matrix-ref knight-table table-x table-y))]
                                             [(pawn? p) (+ 100 (matrix-ref pawn-table table-x table-y))]))))))))))
                    (set! score
                          (+ score
                             (matrix-ref (if (< major-piece-value 8) king-end-table king-middle-table)
                                         king-w-x king-w-y)))
                    (set! score
                          (- score
                             (matrix-ref (if (< major-piece-value 8) king-end-table king-middle-table)
                                         (- 7 king-b-x) (- 7 king-b-y))))
                    (let ([noise 1])
                      (let ([result 
                             (+ score
                                (random noise)
                                (- (/ noise 2)))])
                        (hashtable-set! evaluation-hashtable hash result)
                        ;;(printf "setting ~d\n" result)
                        #;(let ([previous (hashtable-ref evaluation-hashtable hash #f)])
                        (when (and previous (not (= result previous))) ;
                        (draw-board (state-board state)) ;
                        (printf "matches: ~d\n" hash)))
                        #;(when (= result 639.5)
                        (draw-board (state-board state)) ;
                        (printf "this is the duplicate ~d\n" hash))
                        result)))])))))))

#;(let* ([s (get-position-4-state)])
(draw-board (state-board s))
;;(time (evaluate-material s 0))
(set! counter 0) (set! hash-counter 0)
(time (choose-best-move 6 (get-possible-moves s) s #f))
(printf "evaluated positions not including hash: ~d\ntotal evaluated: ~d\n" counter hash-counter))

(define (evaluate-material-with-no-more-moves state depth)
  (let ([board (state-board state)]
        [color (state-color state)])
    (if (in-check? color board)
        (if (eq? color 'b) (+ 10000 depth) (- -10000 depth))
        0)))

(define (make-best-move-chooser-with-depth depth)
  (lambda (moves state)
    (cadr (choose-best-move depth moves state #f))))

(define (choose-best-move depth moves state first-move-values)
  (let ([evaluation-hashtable (make-eq-hashtable)])
    (call-with-values
        (lambda ()
          (alpha-beta state (eq? (state-color state) 'w) depth -inf.0 +inf.0 #f
                      (if first-move-values
                          (map cdr
                               (sort (lambda (p1 p2)
                                       (> (car p1) (car p2)))
                                     (map cons first-move-values moves)))
                          ;;                        (sort (lambda (m1 m2)
                          ;;                                (> (move-value (car m1))
                          ;;                                   (move-value (car m2))))
                          ;;                              (get-possible-moves state))
                          (get-possible-moves state))
                      evaluation-hashtable))
      (lambda (best-val best-move move-values)
        ;;(printf "info score cp ~d depth ~d\n" (exact (floor best-val)) depth)
        ;;(pretty-print (reverse (map move->algebraic (state-moves state))))
        (cons* best-val best-move move-values)))))

(define (alpha-beta state max? depth alpha beta first-move moves evaluation-hashtable)
  (let ([real first-move]
        [move-values '()])
    (if (= depth 0)
        (values (evaluate-material state depth evaluation-hashtable) first-move)
        (if (null? moves)
            (values (evaluate-material-with-no-more-moves state depth) first-move)
            (if (and (is-repetition? (state-moves state)) first-move)
                (values 0 first-move)
                (let ([value (if max? -inf.0 +inf.0)])
                  (call/cc
                   (lambda (k)
                     (for-each
                      (lambda (move)
                        (call-with-values
                            (let ([new-state (apply-moves-on-new-copy move state)])
                              (lambda () (alpha-beta
                                          new-state
                                          (not max?)
                                          (1- depth)
                                          alpha
                                          beta
                                          (if first-move first-move move)
                                          (if (= depth 1)
                                              '()
                                              (sort (lambda (m1 m2)
                                                      ;;(or (eq? 'capture (move-name (car m1)))
                                                      ;;(eq? 'promotion (move-name (car m1)))
                                                      ;;(eq? 'en-passant (move-name (car m1))))
                                                      (> (move-value (car m1))
                                                         (move-value (car m2))))
                                                    (get-possible-moves new-state)))
                                          evaluation-hashtable)))
                          (lambda (child-val child-first-move)
                            (when (not first-move)
                              (set! move-values (cons child-val move-values))
                              (when ((if max? < >) value child-val)
                                (set! real child-first-move)))
                            (set! value ((if max? max min) value child-val))
                            (if max?
                                (set! alpha (max alpha value))
                                (set! beta (min beta value)))
                            (when (>= alpha beta)
                              (k #f)))))
                      moves)))
                  (if (not first-move)
                      (values value real (reverse move-values))
                      (values value first-move))))))))

(define (play-games n)
  (let helper ([n n] [white-wins 0] [black-wins 0] [stalemates 0])
    (begin
      (set! seed (time-nanosecond (current-time)))
      (random-seed seed))
    (if (= 0 n)
        (printf "white wins: ~d, black wins: ~d, stalemates: ~d\n" white-wins black-wins stalemates)
        (case (play-one-game (get-starting-board-state))
          [(1) (helper (1- n) (1+ white-wins) black-wins stalemates)]
          [(-1) (helper (1- n) white-wins (1+ black-wins) stalemates)]
          [(0) (helper (1- n) white-wins black-wins (1+ stalemates))]))))

(define (position->algebraic position)
  (let ([x (position-x position)]
        [y (position-y position)])
    (let ([f (integer->char (+ 97 x))]
          [s (integer->char (+ 48 (- 8 y)))])
      (string f s))))

(define (move->algebraic move)
  (string-append (position->algebraic (move-from move)) (position->algebraic (move-to move))))

(define (algebraic->move str state)
  (let* ([last-move-from (move-from (state-last-move state))]
         [last-move-to (move-to (state-last-move state))]
         [castle-k-w (state-castle-k-w state)]
         [castle-q-w (state-castle-q-w state)]
         [castle-k-b (state-castle-k-b state)]
         [castle-q-b (state-castle-q-b state)]
         [board (state-board state)]
         [color (state-color state)]
         [next-square-proc (if (eq? color 'w) 1- 1+)])
    (let ([from-str (substring str 0 2)] [to-str (substring str 2 4)])
      (call-with-values (lambda () (algebraic->indices from-str))
        (lambda (from-x from-y)
          (call-with-values (lambda () (algebraic->indices to-str))
            (lambda (to-x to-y)
              (let ([promotion (if (> (string-length str) 4)
                                   (begin ;;(pretty-print (string-ref str 4))
                                     (+
                                      (if (eq? color 'b) 8 0)
                                      (case (string-ref str 4)
                                        [(#\q) white-queen]
                                        [(#\r) white-rook]
                                        [(#\b) white-bishop]
                                        [(#\n) white-knight])))
                                   #f)])
                (append
                 (if (and (pawn? (matrix-ref board from-x from-y))
                          (= (position-x last-move-to) to-x)
                          (= (position-x last-move-from) to-x)
                          (= (position-y last-move-to) from-y)
                          (= (position-y last-move-from) (next-square-proc to-y))
                          (not (= to-x from-x)))
                     (list (make-move (make-position to-x to-y) (make-position to-x from-y) #f #f 0))
                     '())
                 (list (make-move (make-position from-x from-y)
                                  (make-position to-x to-y)
                                  promotion
                                  #f
                                  0))
                 (if (and (string=? "e1g1" str) castle-k-w)
                     (list (make-move (make-position 7 7) (make-position 5 7) #f #f 0))
                     '())
                 (if (and (string=? "e1c1" str) castle-q-w)
                     (list (make-move (make-position 0 7) (make-position 3 7) #f #f 0))
                     '())
                 (if (and (string=? "e8g8" str) castle-k-b)
                     (list (make-move (make-position 7 0) (make-position 5 0) #f #f 0))
                     '())
                 (if (and (string=? "e8c8" str) castle-q-b)
                     (list (make-move (make-position 0 0) (make-position 3 0) #f #f 0))
                     '()))))))))))

#;(let ([s (get-starting-board-state)])
(for-each
(lambda (str)
(let ([move (algebraic->move str s)])
(newline)
(printf "last move: ~d\n" (state-last-move s))
(pretty-print move)
(set! s (apply-moves move s))))
'("b1c3" "g8f6" "g1f3" "b8c6" "e2e3" "e7e6" "h2h4" "f8b4"
"c3e2" "h8f8" "a2a3" "b4d6" "d2d4" "h7h6" "e2c3" "f6d5"
"c3d5" "e6d5" "c2c4" "d5c4" "f1c4" "d8f6" "d4d5" "c6e5"
"c4b3" "e5f3" "g2f3" "d6e5" "b3c2" "f6b6" "a1b1" "b6a5"
"e1e2" "d7d6" "b2b4" "a5b5" "c2d3" "b5b6" "d1g1" "c8d7"
"f3f4" "e5f6" "g1g3" "d7b5" "e3e4" "a8e8" "g3f3" "e8d8"
"e2f1" "b5d3" "f3d3" "b6a6" "b4b5" "a6a5" "c1d2" "a5b6"
"d2e3" "b6a5" "e3d2" "a5b6" "d2e3" "b6a5" "h4h5" "a5c3"
"f1e2" "c3d3" "e2d3" "a7a6" "b1c1" "c7c5" "b5c6"))
(draw-board (state-board s)))

#;(let ([s (get-starting-board-state)])
(set! counter 0)
(choose-best-move 5 (get-possible-moves s) s)
(printf "counter: ~d\n" counter))

#;(do ((x 0 (1+ x))) ((= x 1000))
(play-one-game (get-starting-board-state)
(make-best-move-chooser-with-depth 5)
(make-best-move-chooser-with-depth 5)))

#;(time (play-one-game (get-starting-board-state)
(make-best-move-chooser-with-depth 5)
(make-best-move-chooser-with-depth 5)))

#;(let ([depth 5])
  (define s (fen->state "r1bq3r/ppp2kpp/2n5/2b1p3/8/P1P2N2/1PP2PPP/R1BQK2R w KQ - 0 9"))
  (set! counter 0)
  (set! hash-counter 0)
  (time (pretty-print (choose-best-move depth (get-possible-moves s) s #f)))
  (printf "~d, ~d\n" counter hash-counter))

#;(let ([s (get-starting-board-state)])
(set! counter 0)
(time
(pretty-print
(call-with-values (lambda ()
(choose-best-move 5 (get-possible-moves s) s
;;#f
'(-0.5 -0.5 14.5 -10.5 9.5 4.5 39.5 59.5 59.5 69.5 19.5 39.5 34.5 39.5 49.5 44.5 69.5 54.5 54.5 69.5)
))
list)))
(pretty-print counter))
