(define big (expt 2 50))

(define zobrist-piece-constants
  (make-vector 64 #f))

(do ((x 0 (1+ x))) ((= x 64))
  (let ([new-vector (make-vector 14 #f)])
    (do ((y 0 (1+ y))) ((= y 14))
      (vector-set! new-vector y (random big)))
    (vector-set! zobrist-piece-constants x new-vector)))

(define zobrist-castle-k-w (random big))
(define zobrist-castle-q-w (random big))
(define zobrist-castle-k-b (random big))
(define zobrist-castle-q-b (random big))

(define zobrist-en-passant-constants
  (make-vector 8 #f))

(do ((x 0 (1+ x))) ((= x 8))
  (vector-set! zobrist-en-passant-constants x (random big)))

(define (hash-state s)
  (set! hash-counter (1+ hash-counter))
  (let ([board (state-board s)]
        [last-move (state-last-move s)]
        [castle-k-w (state-castle-k-w s)]
        [castle-q-w (state-castle-q-w s)]
        [castle-k-b (state-castle-k-b s)]
        [castle-q-b (state-castle-q-b s)])
    (let ([result 0])
      (do ((x 0 (1+ x))) ((= x 8))
        (do ((y 0 (1+ y))) ((= y 8))
          (let ([p (matrix-ref board x y)])
            (when p
              (set! result (bitwise-xor result
                                        (matrix-ref zobrist-piece-constants
                                                    p
                                                    (+ (* 8 x) y))))))))
      (when castle-k-w
        (set! result (bitwise-xor result zobrist-castle-k-w)))
      (when castle-q-w
        (set! result (bitwise-xor result zobrist-castle-q-w)))
      (when castle-k-b
        (set! result (bitwise-xor result zobrist-castle-k-b)))
      (when castle-q-b
        (set! result (bitwise-xor result zobrist-castle-q-b)))
      (let ([last-move-to (move-to last-move)]
            [last-move-from (move-from last-move)])
        (let ([p (matrix-ref board (position-x last-move-to) (position-y last-move-to))])
          (when (and p (pawn? p) (= 2 (abs (- (position-y last-move-to)
                                              (position-y last-move-from)))))
            (set! result (bitwise-xor result (vector-ref zobrist-en-passant-constants
                                                         (position-x last-move-to)))))))
      result)))
