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

;;TODO: promotions, castles, en passants
(define (get-best-move previous-moves)
  (let ([board (state-board (get-starting-board-state))])
    (for-each
     (lambda (move)
       (let ([from-str (substring move 0 2)] [to-str (substring move 2 4)])
         (call-with-values (lambda () (algebraic->indices from-str))
           (lambda (from-x from-y)
             (call-with-values (lambda () (algebraic->indices to-str))
               (lambda (to-x to-y)
                 (matrix-set! board to-x to-y (matrix-ref board from-x from-y))
                 (matrix-set! board from-x from-y #f)))))))
     previous-moves)
    (let ([state (make-state (if (= 0 (mod (length previous-moves) 2)) 'w 'b)
                             empty-move
                             #f
                             #f
                             #f
                             #f
                             board)])
      (let ([moves (get-possible-moves state)])
        (move->algebraic (car (choose-move-minimax-with-depth 4 moves state)))))))

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
