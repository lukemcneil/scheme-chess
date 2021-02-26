(random-seed (time-nanosecond (current-time)))

(define-record-type position
  (fields x y))

(define-record-type move
  (fields from to promotion))

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

(define (get-starting-board)
  (vector (vector black-rook black-knight black-bishop black-queen black-king black-bishop black-knight black-rook)
          (make-vector 8 black-pawn)
          (make-vector 8 #f)
          (make-vector 8 #f)
          (make-vector 8 #f)
          (make-vector 8 #f)
          (make-vector 8 white-pawn)
          (vector white-rook white-knight white-bishop white-queen white-king white-bishop white-knight white-rook)))

(define (get-testing-board)
  (vector (make-vector 8 #f)
          (make-vector 8 white-pawn)
          (make-vector 8 #f)
          (make-vector 8 #f)
          (make-vector 8 #f)
          (make-vector 8 #f)
          (make-vector 8 black-pawn)
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

(define (get-possible-moves color last-move-x last-move-y board)
  (apply append
         (apply append
                (map
                 (lambda (row y)
                   (map
                    (lambda (p x)
                      (let ([check? (if (eq? color 'w) white? black?)])
                        (if (and p (check? p))
                            (cond
                             [(king? p) (get-king-moves x y color board)]
                             [(queen? p) (get-sliding-piece-moves x y color board
                                                                  '((1 0) (0 1) (-1 0) (0 -1)
                                                                    (1 1) (1 -1) (-1 1) (-1 -1)))]
                             [(rook? p) (get-sliding-piece-moves x y color board
                                                                 '((1 0) (0 1) (-1 0) (0 -1)))]
                             [(bishop? p) (get-sliding-piece-moves x y color board
                                                                   '((1 1) (1 -1) (-1 1) (-1 -1)))]
                             [(knight? p) (get-knight-moves x y color board)]
                             [(pawn? p) (get-pawn-moves x y last-move-x last-move-y color board)])
                            '())))
                    (vector->list row)
                    (iota (vector-length row))))
                 (vector->list board)
                 (iota (vector-length board))))))

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
                                       #f))
                      (helper directions new-x new-y))]
               [(is-color? (matrix-ref board new-x new-y) color)
                (helper (cdr directions) x y)]
               [else ;; other color
                (cons (list (make-move (make-position x y)
                                       (make-position new-x new-y)
                                       #f))
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
               (list (make-move (make-position x y) (make-position new-x new-y) #f))
               (helper (cdr possibilities)))
              (helper (cdr possibilities)))))))

(define (get-king-moves x y color board)
  (let helper ([possibilities '((-1 -1) (-1 0) (-1 1) (0 -1) (0 1) (1 -1) (1 0) (1 1))])
    (if (null? possibilities)
        '()
        (let ([new-x (+ x (caar possibilities))]
              [new-y (+ y (cadar possibilities))])
          (if (square-empty-or-capturable-and-on-board? new-x new-y color board)
              (cons
               (list (make-move (make-position x y) (make-position new-x new-y) #f))
               (helper (cdr possibilities)))
              (helper (cdr possibilities)))))))

(define (get-promotion-list from-x from-y to-x to-y color)
  (list (list (make-move (make-position from-x from-x)
                         (make-position to-x to-y)
                         (+ (if (eq? color 'w) 0 8)
                            white-queen)))
        (list (make-move (make-position from-x from-y)
                         (make-position to-x to-y)
                         (+ (if (eq? color 'w) 0 8)
                            white-rook)))
        (list (make-move (make-position from-x from-y)
                         (make-position to-x to-y)
                         (+ (if (eq? color 'w) 0 8)
                            white-knight)))
        (list (make-move (make-position from-x from-y)
                         (make-position to-x to-y)
                         (+ (if (eq? color 'w) 0 8)
                            white-bishop)))))

(define (get-pawn-moves x y last-move-x last-move-y color board)
  (let ([next-square-proc (if (eq? color 'w) 1- 1+)]
        [starting-square (if (eq? color 'w) 6 1)]
        [en-passant-square (if (eq? color 'w) 3 4)]
        [promotion-square (if (eq? color 'w) 0 7)])
    (append
     (if (square-empty-and-on-board? x (next-square-proc y) board)
         (if (= (next-square-proc y) promotion-square)
             (get-promotion-list x y x (next-square-proc y) color)
             (list (list (make-move (make-position x y)
                                    (make-position x (next-square-proc y))
                                    #f))))
         '())
     (if (and (= y starting-square)
              (square-empty-and-on-board? x (next-square-proc (next-square-proc y)) board)
              (square-empty-and-on-board? x (next-square-proc y) board))
         (list (list (make-move (make-position x y)
                                (make-position x (next-square-proc (next-square-proc y)))
                                #f)))
         '())
     (if (square-capturable-and-on-board? (1+ x) (next-square-proc y) color board)
         (if (= (next-square-proc y) promotion-square)
             (get-promotion-list x y (1+ x) (next-square-proc y) color)
             (list (list (make-move (make-position x y)
                                    (make-position (1+ x) (next-square-proc y))
                                    #f))))
         '())
     (if (square-capturable-and-on-board? (1- x) (next-square-proc y) color board)
         (if (= (next-square-proc y) promotion-square)
             (get-promotion-list x y (1- x) (next-square-proc y) color)
             (list (list (make-move (make-position x y)
                                    (make-position (1- x) (next-square-proc y))
                                    #f))))
         '())
     (if (and (= y en-passant-square)
              (= last-move-x (1+ x))
              (= last-move-y y)
              (square-nonempty-and-on-board? (1+ x) y board)
              (pawn? (matrix-ref board (1+ x) y))
              (not (is-color? (matrix-ref board (1+ x) y) color)))
         (list (list
                (make-move (make-position (1+ x) (next-square-proc y))
                           (make-position (1+ x) y)
                           #f)
                (make-move (make-position x y)
                           (make-position (1+ x) (next-square-proc y))
                           #f)))
         '())
     (if (and (= y en-passant-square)
              (= last-move-x (1- x))
              (= last-move-y y)
              (square-nonempty-and-on-board? (1- x) y board)
              (pawn? (matrix-ref board (1- x) y))
              (not (is-color? (matrix-ref board (1- x) y) color)))
         (list (list
                (make-move (make-position (1- x) (next-square-proc y))
                           (make-position (1- x) y)
                           #f)
                (make-move (make-position x y)
                           (make-position (1- x) (next-square-proc y))
                           #f)))
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

(define (apply-moves moves board)
  (unless (null? moves)
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
      (apply-moves (cdr moves) board))))

(define (do-one-move color last-move-x last-move-y board)
  (let ([moves (get-possible-moves color last-move-x last-move-y board)])
    (if (null? moves)
        (printf "no more moves\n")
        (let ([random-move (list-ref moves (random (length moves)))])
          (apply-moves random-move board)
          (draw-board board)
          (sleep (make-time 'time-duration 10000000 0))
          (do-one-move (if (eq? color 'w) 'b 'w) 0 0 board)))))

#;(get-pawn-moves 5 1 'b (get-starting-board))

(define b (get-testing-board))
(matrix-set! b 5 0 black-bishop)
(matrix-set! b 5 1 white-bishop)
;;(matrix-set! b 2 3 black-pawn)
;;(draw-board b)
;;(do-one-move 'b 0 3 b)

;;(length (get-possible-moves 'b 0 0 (get-starting-board)))

#;(play-game (get-starting-board))

(do-one-move 'w 0 0 (get-starting-board))
