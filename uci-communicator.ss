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

(define (get-best-move previous-moves)
  (printf "info get best move with ~d\n" previous-moves)
  (let ([state (get-starting-board-state)])
    (for-each
     (lambda (move)
       (set! state (apply-moves (algebraic->move move state) state)))
     previous-moves)
    (display "info ")
    (draw-board (state-board state))
    (let ([moves (get-possible-moves state)])
      (let ([best (choose-best-move 5 moves state)])
        (string-append
         (move->algebraic ((if (eq? 'en-passant (move-name (car best))) cadr car)
                           best))
         (case (move-promotion (car best))
           [(1 9) "q"]
           [(2 10) "r"]
           [(3 11) "b"]
           [(4 12) "n"]
           [else ""]))))))

(let ([moves '()])
  (let loop ()
    (let ([input (string-split (get-line (current-input-port)))])
      (case (car input)
        [("uci") (display "uciok\n")]
        [("isready") (display "readyok\n")]
        [("position")
         (if (string=? (cadr input) "startpos")
             (if (and (not (null? (cddr input))) (string=? (caddr input) "moves"))
                 (set! moves (cdddr input))
                 (set! moves '())))]
        [("go")
         (display (string-append "bestmove " (get-best-move moves) "\n"))]
        [else (printf "info unrecognized command ~d\n" (car input))])
      (loop))))
