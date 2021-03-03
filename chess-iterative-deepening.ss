(define-record-type node
  (fields state moves children))

(let ([s (get-starting-board-state)])
  (let ([moves (get-possible-moves s)])
    (let ([n (make-node s
                        moves
                        (map (lambda (move) (make-node (apply-moves-on-new-copy move s) #f #f))
                             moves))])
      (sort-moves-by-material n))))

(define (sort-moves-by-material node)
  (map
   (lambda (move child)
     child)
   (node-moves node)
   (node-children node)))

(define (choose-new state depth)
  (let ([values (new-recursive state
                               (eq? (state-color state) 'w)
                               depth
                               -inf.0 +inf.0
                               #f)]
        [moves (get-possible-moves state)])
    (map cdr
         (sort
          (lambda (p1 p2) (> (car p1) (car p2)))
          (map cons values moves)))))

(define (new-recursive state max? depth alpha beta first-move)
  (let ([real first-move]
        [values-for-sorting '()])
    (if (= depth 0)
        (begin
          (set! counter (1+ counter))
          (values (evaluate-material state depth)
                  first-move))
        (let ([moves (get-possible-moves state)])
          (if (null? moves)
              (begin
                (set! counter (1+ counter))
                (values (evaluate-material state depth)
                        first-move))
              (let ([value (if max? -inf.0 +inf.0)])
                (call/cc
                 (lambda (k)
                   (for-each
                    (lambda (move)
                      (call-with-values (lambda () (new-recursive
                                                    (apply-moves-on-new-copy move state)
                                                    (not max?)
                                                    (1- depth)
                                                    alpha
                                                    beta
                                                    (if first-move first-move move)))
                        (lambda (child-val child-first-move)
                          (when (not first-move)
                            (set! values-for-sorting (cons child-val values-for-sorting)))
                          (when (and ((if max? < >) value child-val) (not first-move))
                            (set! real child-first-move))
                          (set! value ((if max? max min) value child-val))
                          (if max?
                              (set! alpha (max alpha value))
                              (set! beta (min beta value)))
                          (when (>= alpha beta)
                            (k #f)))))
                    moves)))
                (if (not first-move)
                    values-for-sorting
                    (values value first-move))))))))

(trace new-recursive)
(choose-new (get-starting-board-state) 3)

(let ([depth 4])
  (define s (get-position-6-state))
  (set! counter 0)
  (time
   (pretty-print (choose-move-minimax-alpha-beta-with-depth-one-call depth (get-possible-moves s) s)))
  (pretty-print counter)
  (set! counter 0)
  (time (pretty-print (choose-move-minimax-alpha-beta-with-depth depth (get-possible-moves s) s)))
  (pretty-print counter)
  (set! counter 0)
  #;(choose-move-minimax-with-depth depth (get-possible-moves s) s)
  (pretty-print counter))
