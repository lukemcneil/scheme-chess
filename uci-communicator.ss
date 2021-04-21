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

(define (sleep-seconds seconds)
  (sleep (make-time 'time-duration (exact (floor (* (- seconds (floor seconds)) 1000000000)))
                    (exact (floor seconds)))))

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
      (let ([best #f]
            [furthest-depth #f]
            [furthest-evaluation #f]
            [mutex (make-mutex)]
            [done #f])
        ;; (for-each
        ;;  (lambda (i)
        ;;    (fork-thread
        ;;     (lambda ()
        ;;       (let loop ([engine (make-engine (lambda () (choose-best-move i moves state)))])
        ;;         (mutex-acquire mutex)
        ;;         (if done
        ;;             (mutex-release mutex)
        ;;             (begin
        ;;               (mutex-release mutex)
        ;;               (engine 1000000
        ;;                       (lambda (fuel-left result)
        ;;                         (let ([best-val (car result)] [best-move (cdr result)])
        ;;                           (with-mutex mutex
        ;;                                       #;(printf "info depth ~d\n" i)
        ;;                                       #;(printf "info score cp ~d\n"
        ;;                                               (exact (floor best-val)))
        ;;                                       (set! best best-move)
        ;;                                       (set! furthest-depth i)
        ;;                                       (set! furthest-evaluation best-val))))
        ;;                       (lambda (new-engine)
        ;;                         (loop new-engine)))))))))
        ;;  (map 1+ (iota 8)))
        (set! counter 0)
        (fork-thread
         (lambda ()
           (let loop ([engine (make-engine (lambda () (choose-best-move 1 moves state #f)))]
                      [i 1])
             (mutex-acquire mutex)
             (if done
                 (mutex-release mutex)
                 (begin
                   (mutex-release mutex)
                   (engine 1000000
                           (lambda (fuel-left result)
                             (let ([best-val (car result)]
                                   [best-move (cadr result)]
                                   [move-values (cddr result)])
                               (with-mutex mutex
                                           (printf "info depth ~d\n" i)
                                           (printf "info score cp ~d\n" (exact (floor best-val)))
                                           (printf "info d ~d found best move ~d\n" i
                                                   (move->algebraic (car best-move)))
                                           (set! furthest-evaluation best-val)
                                           (set! best best-move)
                                           (set! furthest-depth i))
                               (loop (make-engine
                                      (lambda () (choose-best-move (1+ i) moves state move-values)))
                                     (1+ i))))
                           (lambda (new-engine)
                             (loop new-engine i))))))))
        (sleep-seconds 1)
        (with-mutex mutex
                    (set! done #t)
                    (printf "info looked at ~d\n" counter)
                    (printf "info depth ~d\n" furthest-depth)
                    (printf "info score cp ~d\n" (exact (floor furthest-evaluation)))
                    (string-append
                     (move->algebraic ((if (eq? 'en-passant (move-name (car best))) cadr car)
                                       best))
                     (case (move-promotion (car best))
                       [(1 9) "q"]
                       [(2 10) "r"]
                       [(3 11) "b"]
                       [(4 12) "n"]
                       [else ""])))))))

(let ([moves '()] [done #f])
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
        [("quit") (set! done #t) (printf "info quitting\n")]
        [else (printf "info unrecognized command ~d\n" (car input))])
      (if done
          (exit)
          (loop)))))
