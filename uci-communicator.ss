#!/usr/bin/scheme

(load "/home/luke/chess/chess-engine.ss")

(define (tokenize l)
  (let loop ((t '())
             (l l))
    (if (pair? l)
        (let ((c (car l)))
          (if (char=? c #\space)
              (cons (reverse t) (loop '() (cdr l)))
              (loop (cons (car l) t) (cdr l))))
        (if (null? t)
            '()
            (list (reverse t))))))

(define (string-split s)
  (map list->string (tokenize (string->list s))))

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
                                   (begin (pretty-print (string-ref str 4))
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
                          (not (= to-x to-y)))
                     (list (make-move (make-position to-x to-y) (make-position to-x from-y) #f #f))
                     '())
                 (list (make-move (make-position from-x from-y)
                                  (make-position to-x to-y)
                                  promotion
                                  #f))
                 (if (and (string=? "e1g1" str) castle-k-w)
                     (list (make-move (make-position 7 7) (make-position 5 7) #f #f))
                     '())
                 (if (and (string=? "e1c1" str) castle-q-w)
                     (list (make-move (make-position 0 7) (make-position 3 7) #f #f))
                     '())
                 (if (and (string=? "e8g8" str) castle-k-b)
                     (list (make-move (make-position 7 0) (make-position 5 0) #f #f))
                     '())
                 (if (and (string=? "e8c8" str) castle-q-b)
                     (list (make-move (make-position 0 0) (make-position 3 0) #f #f))
                     '()))))))))))

#;(begin
  (define b (get-starting-board-state))
  (set! b (apply-moves (algebraic->move "e2e4" b) b))
  (draw-board (state-board b))
  (set! b (apply-moves (algebraic->move "a7a6" b) b))
  (draw-board (state-board b))
  (set! b (apply-moves (algebraic->move "e4e5" b) b))
  (draw-board (state-board b))
  (set! b (apply-moves (algebraic->move "d7d5" b) b))
  (draw-board (state-board b))
  (set! b (apply-moves (algebraic->move "e5d6" b) b))
  (draw-board (state-board b)))

(algebraic->move "e1g1" (get-starting-board-state))

(define (get-best-move previous-moves)
  (let ([state (get-starting-board-state)])
    (for-each
     (lambda (move)
       (set! state (apply-moves (algebraic->move move state) state)))
     previous-moves)
    (display "info ")
    (draw-board (state-board state))
    (let ([moves (get-possible-moves state)])
      (let ([best (choose-move-minimax-alpha-beta-with-depth 4 moves state)])
        (string-append
         (move->algebraic ((if (eq? 'en-passant (move-name (car best))) cadr car)
                           best))
         (case (move-promotion (car best))
           [(1 9) "q"]
           [(2 10) "r"]
           [(3 11) "b"]
           [(4 12) "n"]
           [else ""]))))))

(let loop ()
  (let ([input (string-split (get-line (current-input-port)))])
    (case (car input)
      [("uci") (display "uciok\n")]
      [("isready") (display "readyok\n")]
      [("position")
       (if (string=? (cadr input) "startpos")
           (if (and (not (null? (cddr input))) (string=? (caddr input) "moves"))
               (display (string-append "bestmove " (get-best-move (cdddr input)) "\n"))
               (display (string-append "bestmove " (get-best-move '()) "\n"))))])
    (loop)))
