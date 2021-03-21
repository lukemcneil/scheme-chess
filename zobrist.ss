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

(define zobrist-white (random big))

(define zobrist-en-passant-constants
  (make-vector 8 #f))

(do ((x 0 (1+ x))) ((= x 8))
  (vector-set! zobrist-en-passant-constants x (random big)))

(define (get-hash board last-move castle-k-w castle-q-w castle-k-b castle-q-b color)
  (set! hash-counter (1+ hash-counter))
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
    (when (eq? color 'w)
      (set! result (bitwise-xor result zobrist-white)))
    result))

(define (apply-moves-to-hash moves board hash)
  ;;(printf "getting new hash from ~d\n" hash)
  (if (null? moves)
      (bitwise-xor hash zobrist-white)
      (apply-moves-to-hash
       (cdr moves) board
       (let* ([move (car moves)]
              [move-from (move-from move)]
              [move-to (move-to move)]
              [moving-p (matrix-ref board (position-x move-from) (position-y move-from))]
              [taken-p (matrix-ref board (position-x move-to) (position-y move-to))])
         (if (not moving-p)
             (begin
               (if taken-p
                   (bitwise-xor
                    hash
                    (matrix-ref zobrist-piece-constants taken-p (+ (* 8 (position-x move-to))
                                                                   (position-y move-to))))
                   hash))
             (bitwise-xor
              hash
              (matrix-ref zobrist-piece-constants moving-p (+ (* 8 (position-x move-from))
                                                              (position-y move-from)))
              (matrix-ref zobrist-piece-constants moving-p (+ (* 8 (position-x move-to))
                                                              (position-y move-to)))
              (if taken-p
                  (matrix-ref zobrist-piece-constants taken-p (+ (* 8 (position-x move-to))
                                                                 (position-y move-to)))
                  0)))))))

#;(let ([s (fen->state "8/8/8/8/7K/8/7k/8 w - - 0 9")])
  (draw-board (state-board s))
  (printf "hash: ~d\n" (get-hash (state-board s) (state-last-move s) (state-castle-k-w s)
                                 (state-castle-q-w s) (state-castle-k-b s) (state-castle-q-b s)
                                 (state-color s)))
  (printf "hash: ~d\n" (state-hash s))
  (pretty-print (apply-moves-to-hash
                 (list (make-move (make-position 7 4) (make-position 7 3) #f #f 0))
                 (state-board s)
                 579671985817313))
  (set! s (apply-moves (list (make-move (make-position 7 4) (make-position 7 3) #f #f 0)) s))
  (draw-board (state-board s))
  (printf "hash: ~d\n" (get-hash (state-board s) (state-last-move s) (state-castle-k-w s)
                                 (state-castle-q-w s) (state-castle-k-b s) (state-castle-q-b s)
                                 (state-color s)))
  (printf "hash: ~d\n" (state-hash s))
  (printf "correct: ~d\n" (bitwise-xor 579671985817313
                                       (matrix-ref zobrist-piece-constants 0 (+ (* 8 7) 4))
                                       (matrix-ref zobrist-piece-constants 0 (+ (* 8 7) 3))))
  )
