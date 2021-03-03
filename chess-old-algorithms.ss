(define (choose-random-move moves current-state)
  (list-ref moves (random (length moves))))

(define (choose-best-material-move moves state)
  (let ([color (state-color state)])
    (let helper ([moves (cdr moves)]
                 [m (evaluate-material (apply-moves-on-new-copy (car moves) state))]
                 [best-move (car moves)]
                 [best-moves (list (car moves))])
      (if (null? moves)
          (choose-random-move best-moves state)
          (let* ([new-state (apply-moves-on-new-copy (car moves) state)]
                 [eval (evaluate-material new-state 0)])
            (cond
             [((if (eq? color 'w) > <) eval m)
              (helper (cdr moves) eval (car moves) (list (car moves)))]
             [(= eval m) (helper (cdr moves) m best-move (cons (car moves) best-moves))]
             [else (helper (cdr moves) m best-move best-moves)]))))))

(define (choose-human-move moves current-state)
  (display "from: ")
  (call-with-values (lambda () (algebraic->indices (symbol->string (read))))
    (lambda (from-x from-y)
      (display "to: ")
      (call-with-values (lambda () (algebraic->indices (symbol->string (read))))
        (lambda (to-x to-y)
          (list (make-move (make-position from-x from-y)
                           (make-position to-x to-y)
                           #f
                           #f)))))))


(define (make-minimax-chooser-with-depth depth)
  (lambda (moves state)
    (choose-move-minimax-with-depth depth moves state)))

(define (choose-move-minimax-with-depth depth moves state)
  (define (get-best color ls)
    (let ([compare (if (eq? color 'w) > <)])
      (let helper ([ls (cdr ls)]
                   [m (caar ls)]
                   [best-moves (list (cdar ls))])
        (cond
         [(null? ls) (printf "evaluation from ~d: ~d\n" (state-color state) m)
          (choose-random-move best-moves state)
          ;;best-moves
          ]
         [(compare (caar ls) m) (helper (cdr ls) (caar ls) (list (cdar ls)))]
         [(= (caar ls) m) (helper (cdr ls) m (cons (cdar ls) best-moves))]
         [else (helper (cdr ls) m best-moves)]))))
  moves
  (get-best (state-color state)
            (map
             (lambda (move)
               (cons (evaluate-material-recursively
                      (apply-moves-on-new-copy move state)
                      (if (eq? (state-color state) 'w) 'min 'max)
                      (1- depth))
                     move))
             moves)))

(define (evaluate-material-recursively state max-or-min depth)
  (if (= depth 0)
      (begin
        (set! counter (1+ counter))
        (evaluate-material state depth))
      (let ([moves (get-possible-moves state)])
        (if (null? moves)
            (begin
              (set! counter (1+ counter))
              (evaluate-material-with-no-more-moves state depth))
            (apply (if (eq? max-or-min 'max) max min)
                   (map
                    (lambda (move) (evaluate-material-recursively
                                    (apply-moves-on-new-copy move state)
                                    (if (eq? max-or-min 'max) 'min 'max)
                                    (1- depth)))
                    moves))))))

(define (make-minimax-alpha-beta-chooser-with-depth depth)
  (lambda (moves state)
    (choose-move-minimax-alpha-beta-with-depth depth moves state)))

(define (choose-move-minimax-alpha-beta-with-depth depth moves state)
  (define (get-best color ls)
    (let ([compare (if (eq? color 'w) > <)])
      (let helper ([ls (cdr ls)]
                   [m (caar ls)]
                   [best-moves (list (cdar ls))])
        (cond
         [(null? ls) (printf "info evaluation from alpha beta ~d: ~d\n" (state-color state) m)
          (choose-random-move best-moves state)
          ;;best-moves
          ]
         [(compare (caar ls) m) (helper (cdr ls) (caar ls) (list (cdar ls)))]
         [(= (caar ls) m) (helper (cdr ls) m (cons (cdar ls) best-moves))]
         [else (helper (cdr ls) m best-moves)]))))
  (get-best (state-color state)
            (map
             (lambda (move)
               (cons (alpha-beta
                      (apply-moves-on-new-copy move state)
                      (if (eq? (state-color state) 'w) #f #t)
                      (1- depth)
                      -inf.0
                      +inf.0)
                     move))
             moves)))

(define (alpha-beta state max? depth alpha beta)
  (if (= depth 0)
      (begin
        (set! counter (1+ counter))
        (evaluate-material state depth))
      (let ([moves (get-possible-moves state)])
        (if (null? moves)
            (begin
              (set! counter (1+ counter))
              (evaluate-material-with-no-more-moves state depth))
            (let ([value (if max? -inf.0 +inf.0)])
              (call/cc
               (lambda (k)
                 (for-each
                  (lambda (move)
                    (set! value ((if max? max min) value
                                 (alpha-beta
                                  (apply-moves-on-new-copy move state)
                                  (not max?)
                                  (1- depth)
                                  alpha
                                  beta)))
                    (if max?
                        (set! alpha (max alpha value))
                        (set! beta (min beta value)))
                    (when (>= alpha beta)
                          (k #f)))
                  (sort (lambda (m1 m2) (eq? 'capture (move-name (car m1)))) moves))))
              value)))))
