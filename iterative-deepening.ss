(define (make-node state value moves children evaluation)
  (let ([h (make-eq-hashtable)])
    (hashtable-set! h 'state state)
    (hashtable-set! h 'value value)
    (hashtable-set! h 'moves moves)
    (hashtable-set! h 'children children)
    (hashtable-set! h 'evaluation evaluation)
    h))

(define (node-state node)
  (hashtable-ref node 'state #f))
(define (node-value node)
  (hashtable-ref node 'value #f))
(define (node-moves node)
  (hashtable-ref node 'moves #f))
(define (node-children node)
  (hashtable-ref node 'children #f))
(define (node-evaluation node)
  (hashtable-ref node 'evaluation #f))

(define (node-set-state! node val)
  (hashtable-set! node 'state val))
(define (node-set-value! node val)
  (hashtable-set! node 'value val))
(define (node-set-moves! node val)
  (hashtable-set! node 'moves val))
(define (node-set-children! node val)
  (hashtable-set! node 'children val))
(define (node-set-evaluation! node val)
  (hashtable-set! node 'evaluation val))

(define (evaluate! node)
  (unless (node-evaluation node)
    (node-set-evaluation! node (evaluate-material (node-state node) 0))))

(define (get-moves-and-children! node)
  (unless (node-moves node)
    (let ([moves (get-possible-moves (node-state node))])
      (node-set-moves! node moves)
      (node-set-children! node (map (lambda (move) (make-node
                                                    (apply-moves-on-new-copy move (node-state node))
                                                    #f
                                                    #f
                                                    #f
                                                    #f))
                                    moves)))))

(define (node-print node)
  (printf "state:")
  (draw-board (state-board (node-state node)))
  (printf "value: ~d\nevaluation: ~d\nfirst move: ~d\nfirst child state:"
          (node-value node) (node-evaluation node) (car (node-moves node)))
  (draw-board (state-board (node-state (car (node-children node))))))

(define (sort-moves-and-children node)
  (let ([moves (node-moves node)]
        [children (node-children node)])
    (for-each
     (lambda (child)
       (evaluate! child))
     children)
    (let ([sorted (sort (lambda (p1 p2) (or (eq? (move-name (caar p1)) 'capture)
                                            (eq? (move-name (caar p1)) 'promotion)))#;(lambda (p1 p2) (> (caddr p1) (caddr p2)))
                        (map list moves children
                             (map (lambda (child) (node-evaluation child)) children)))])
      (node-set-moves! node (map car sorted))
      (node-set-children! node (map cadr sorted)))))

(define (choose-best-move-new depth moves state)
  (call-with-values (lambda () (alpha-beta-new state (eq? (state-color state) 'w) depth -inf.0 +inf.0 #f))
    (lambda (best-val best-move)
      (printf "info evaluation from one call ~d: ~d\n" (state-color state) best-val)
      best-move)))

(define (alpha-beta-new node depth alpha beta max?)
  (if (= 0 depth)
      (begin
        (evaluate! node)
        (node-evaluation node))
      (begin
        (get-moves-and-children! node)
        (if (null? (node-moves node))
            (begin
              (evaluate! node)
              (node-evaluation node))
            (let ([value (if max? -inf.0 +inf.0)])
              (call/cc
               (lambda (k)
                 (sort-moves-and-children node)
                 (for-each
                  (lambda (child)
                    (set! value ((if max? max min)
                                 value
                                 (alpha-beta-new child (1- depth) alpha beta (not max?))))
                    (if max?
                        (set! alpha (max alpha value))
                        (set! beta (min beta value)))
                    (when (>= alpha beta)
                      (k #f)))
                  (node-children node))))
              value)))))

(let* ([s (get-position-3-state)]
       [moves (get-possible-moves s)]
       [n (make-node s #f #f;;moves
                     #f#;(map (lambda (move) (make-node (apply-moves-on-new-copy move s) #f #f #f #f))
                          moves)
                     #f)])
  #;(sort-moves-and-children n)
  #;(node-print n)
  (set! counter 0)
  (alpha-beta-new n 5 -inf.0 +inf.0 #t)
  (pretty-print counter)
  (set! counter 0)
  (alpha-beta s (eq? (state-color s) 'w) 5 -inf.0 +inf.0 #f)
  (pretty-print counter))
