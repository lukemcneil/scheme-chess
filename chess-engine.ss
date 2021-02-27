(begin
  (define seed (time-nanosecond (current-time)))
  (random-seed seed))
;;(random-seed 107)

(define-record-type position
  (fields x y))

(define-record-type move
  (fields from to promotion name))

(define empty-move (make-move (make-position 0 0) (make-position 0 0) #f #f))

(define-record-type state
  (fields color last-move castle-k-w castle-q-w castle-k-b castle-q-b board))

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

(load "chess-engine-tests.ss")

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
                        " \033[0m")))
          ))
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
                   (iota (vector-length board)))))))

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
                                       'move))
                      (helper directions new-x new-y))]
               [(is-color? (matrix-ref board new-x new-y) color)
                (helper (cdr directions) x y)]
               [else ;; other color
                (cons (list (make-move (make-position x y)
                                       (make-position new-x new-y)
                                       #f
                                       'capture))
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
               (list (make-move (make-position x y) (make-position new-x new-y) #f 'unknown))
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
                (list (make-move (make-position x y) (make-position new-x new-y) #f 'unknown))
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
         (begin
           ;;(set! castle-count (1+ castle-count))
           ;;(draw-board board)
           ;;(printf "~d can castle king side\n" (if (eq? color 'w) "white" "black"))
           ;;(read)
           (list (list (make-move (make-position x y) (make-position (+ 2 x) y) #f 'castle)
                       (make-move (make-position (+ 3 x) y) (make-position (+ 1 x) y) #f 'castle))))
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
         (begin
           ;;(set! castle-count (1+ castle-count))
           ;;(draw-board board)
           ;;(printf "~d can castle queen side\n" (if (eq? color 'w) "white" "black"))
           ;;(read)
           (list (list (make-move (make-position x y) (make-position (- x 2) y) #f 'castle)
                       (make-move (make-position (- 4 x) y) (make-position (1- x) y) #f 'castle))))
         '()))))

(define (get-promotion-list from-x from-y to-x to-y color)
  (list (list (make-move (make-position from-x from-y)
                         (make-position to-x to-y)
                         (+ (if (eq? color 'w) 0 8)
                            white-queen)
                         'promotion))
        (list (make-move (make-position from-x from-y)
                         (make-position to-x to-y)
                         (+ (if (eq? color 'w) 0 8)
                            white-rook)
                         'promotion))
        (list (make-move (make-position from-x from-y)
                         (make-position to-x to-y)
                         (+ (if (eq? color 'w) 0 8)
                            white-knight)
                         'promotion))
        (list (make-move (make-position from-x from-y)
                         (make-position to-x to-y)
                         (+ (if (eq? color 'w) 0 8)
                            white-bishop)
                         'promotion))))

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
             (get-promotion-list x y x (next-square-proc y) color)
             (list (list (make-move (make-position x y)
                                    (make-position x (next-square-proc y))
                                    #f
                                    'move))))
         '())
     (if (and (= y starting-square)
              (square-empty-and-on-board? x (next-square-proc (next-square-proc y)) board)
              (square-empty-and-on-board? x (next-square-proc y) board))
         (list (list (make-move (make-position x y)
                                (make-position x (next-square-proc (next-square-proc y)))
                                #f
                                'move)))
         '())
     (if (square-capturable-and-on-board? (1+ x) (next-square-proc y) color board)
         (if (= (next-square-proc y) promotion-square)
             (get-promotion-list x y (1+ x) (next-square-proc y) color)
             (list (list (make-move (make-position x y)
                                    (make-position (1+ x) (next-square-proc y))
                                    #f
                                    'capture))))
         '())
     (if (square-capturable-and-on-board? (1- x) (next-square-proc y) color board)
         (if (= (next-square-proc y) promotion-square)
             (get-promotion-list x y (1- x) (next-square-proc y) color)
             (list (list (make-move (make-position x y)
                                    (make-position (1- x) (next-square-proc y))
                                    #f
                                    'capture))))
         '())
     (if (and (= y en-passant-square)
              (= last-move-to-x (1+ x))
              (= last-move-to-y y)
              (= last-move-from-y (next-square-proc (next-square-proc y)))
              (square-nonempty-and-on-board? (1+ x) y board)
              (pawn? (matrix-ref board (1+ x) y))
              (not (is-color? (matrix-ref board (1+ x) y) color)))
         (begin
           ;;(set! en-passant-count (1+ en-passant-count))
           ;;(draw-board board)
           (list (list
                  (make-move (make-position (1+ x) (next-square-proc y))
                             (make-position (1+ x) y)
                             #f
                             'en-passant)
                  (make-move (make-position x y)
                             (make-position (1+ x) (next-square-proc y))
                             #f
                             'en-passant))))
         '())
     (if (and (= y en-passant-square)
              (= last-move-to-x (1- x))
              (= last-move-to-y y)
              (= last-move-from-y (next-square-proc (next-square-proc y)))
              (square-nonempty-and-on-board? (1- x) y board)
              (pawn? (matrix-ref board (1- x) y))
              (not (is-color? (matrix-ref board (1- x) y) color)))
         (begin
           ;;(set! en-passant-count (1+ en-passant-count))
           ;;(draw-board board)
           (list (list
                  (make-move (make-position (1- x) (next-square-proc y))
                             (make-position (1- x) y)
                             #f
                             'en-passant)
                  (make-move (make-position x y)
                             (make-position (1- x) (next-square-proc y))
                             #f
                             'en-passant))))
         '()))))

(define (play-game board)
  (draw-board board)
  (display "from:")
  (let ([from (symbol->string (read))])
    (display "to:")
    (let ([to (symbol->string (read))])
      (call-with-values (lambda () (algebraic-to-indices from))
        (lambda (x y)
          (call-with-values (lambda () (algebraic-to-indices to))
            (lambda (new-x new-y)
              (matrix-set! board new-x new-y (matrix-ref board x y))
              (matrix-set! board x y #f)
              (play-game board))))))))

(define (algebraic-to-indices str)
  (let ([first (string-ref str 0)]
        [second (string-ref str 1)])
    (values
     (char- first #\a)
     (- 8 (- (char->integer second) 48)))))

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

(define (apply-moves moves state)
  (call-with-values (lambda () (get-changed-castle-info (car moves)))
    (lambda (k-w q-w k-b q-b)
      (let helper ([moves moves]
                   [last-move #f])
        (let ([board (state-board state)]
              [color (state-color state)]
              [castle-k-w (state-castle-k-w state)]
              [castle-q-w (state-castle-q-w state)]
              [castle-k-b (state-castle-k-b state)]
              [castle-q-b (state-castle-q-b state)])
          (if (null? moves)
              (make-state (if (eq? color 'w) 'b 'w)
                          last-move
                          (if castle-k-w k-w #f)
                          (if castle-q-w q-w #f)
                          (if castle-k-b k-b #f)
                          (if castle-q-b q-b #f)
                          board)
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
                (helper (cdr moves) (car moves)))))))))

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
         [new-state (make-state (state-color state) (state-last-move state)
                                (state-castle-k-w state) (state-castle-q-w state)
                                (state-castle-k-b state) (state-castle-q-b state) new-board)])
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

(define (do-one-move state)
  (let helper ([state state] [move-count 0])
    (if (insufficient-material? state)
        (printf "Stalemate because of insufficient material!!!\n")
        (let* ([board (state-board state)]
               [color (state-color state)]
               [moves (filter
                       (lambda (move)
                         (not (in-check? color (state-board (apply-moves-on-new-copy move state)))))
                       (get-possible-moves state))])
          (if (null? moves)
              (if (in-check? color board)
                  (printf "Checkmate on ~d!!!\n" (if (equal? color 'w) "white" "black"))
                  (printf "Stalemate because ~d has no moves!!!\n"
                          (if (equal? color 'w) "white" "black")))
              (let ([random-move (list-ref moves (random (length moves)))])
                (let ([new-state (apply-moves random-move state)])
                  (display "\033c")
                  ;;(pretty-print random-move)
                  (draw-board board)
                  ;;(printf "possible moves: ~d\n" (length moves))
                  ;;(printf "move number: ~d\n" move-count)
                  ;;(sleep (make-time 'time-duration 1000000 0))
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
         [op-color (if (eq? color 'w) 'b 'w)]
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
  ;; find king position
  (let ([king-position (get-king-position color board)])
    (if (not king-position)
        (printf "there is no king seed: ~d\n" seed)
        (let ([king-x (position-x king-position)]
              [king-y (position-y king-position)])
          (square-under-attack? king-x king-y color board)))))

(define (get-possibilities starting-state starting-depth)
  (let helper ([to-search (list (cons starting-state starting-depth))] [total 0])
    (if (null? to-search)
        total
        (let ([state (caar to-search)]
              [depth (cdar to-search)])
          (if (= 0 depth)
              0
              (let ([color (state-color state)])
                (let ([moves (filter
                              (lambda (move)
                                (not (in-check? color (state-board (apply-moves-on-new-copy move state)))))
                              (get-possible-moves state))])
                  (if (= 1 depth)
                      (begin
                        (for-each
                         (lambda (move)
                           ;;                           (display "\033c")
                           ;;                           (draw-board (state-board (apply-moves-on-new-copy move state)))
                           ;;                           (sleep (make-time 'time-duration 10000000 0))
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

(do-one-move (get-starting-board-state))
