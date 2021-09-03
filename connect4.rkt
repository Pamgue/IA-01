#lang racket

(define v (make-vector 42))
(define celds (make-vector 7))
(define turn 1)
(define tokens 0)
(define game-status #t) 

;#(0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0) se usa un vector unidimensional para representar el tablero
;; 0: celdas vacias
;; 1: piezas p1
;; 2: piezas p2

(define (game-loop) ;;pasar turno inicial
  (cond
    [(equal? #f game-status) 0]
    [else (game-loop-aux)]))

(define (game-loop-aux)
  (cond
    [(equal? turn 1) (print-board v)
                     (displayln "Turno Jugador, elija columna 0-6")
                     (define token (read))       
                     (check-valid-move token)
                     (cond [(equal? (check-win) 1)(set! game-status #f) (displayln "Jugador gana")]
                           [(equal? (check-win) 2)(set! game-status #f) (displayln "IA gana")]
                           [(equal? (check-win) -1)(set! game-status #f) (displayln "Empate")]
                           [else (change-turn) (game-loop)])]
    [else
           (print-board v)
           (displayln "Turno IA, elija columna 0-6")
           (define token (read))       
           (check-valid-move token)
           (cond [(equal? (check-win) 1)(set! game-status #f) (displayln "Jugador gana")]
                    [(equal? (check-win) 2)(set! game-status #f) (displayln "IA gana")]
                    [(equal? (check-win) -1)(set! game-status #f) (displayln "Empate")]
                    [else (change-turn) (game-loop)])]))



(define (update-game-board col)
  (vector-set! v (+ (* 7 (vector-ref celds col)) col) turn))
   

(define (check-valid-move col)
  (cond
   [(equal? (vector-ref celds col) 5) #f] ;;retorna falso si la columna ya esta llena de fichas
   [else (update-game-board col)
         (vector-set! celds col (add1 (vector-ref celds col)))
         (set! tokens (add1 tokens)) #t])) ;;retorna true si el movimiento es valido y actualiza el tope de la columna del juego

(define (change-turn);;cambiar el turno 1: IA 2: jugador
  (if (equal? turn 1)
      (set! turn 2)
      (set! turn 1)))

(define (check-4-horizontal board)
  (define player 0); si gana p1->1,p2->2, sino 0
  (define pos 0)
  (for (
        [i (in-range 6)])
    (for (
        [j (in-range 4)])
      (set! pos (+ (* i 7) j))
      (when (and
             (or (= (vector-ref board pos) 1) (= (vector-ref board pos) 2))
             (= (vector-ref board pos) (vector-ref board (+ 1 pos)))
             (= (vector-ref board (+ 1 pos)) (vector-ref board (+ 2 pos)))
             (= (vector-ref board (+ pos 2)) (vector-ref board (+ 3 pos))))
        (set! player (vector-ref board pos)))))
  player)

(define (check-tie)
  (cond
    [(equal? tokens 42) #t]
    [else #f]))
 
(define (check-win);;empate: -1, jugador 1, IA: 2
  (define horizontal (check-4-horizontal v))
  (define vertical (check-4-vertical v))
  (define d1 (check-4-main-diagonal v))
  (define d2 (check-4-second-diagonal v))
  (cond
    [(or (equal? 1 horizontal) (equal? 2 horizontal)) horizontal]
    [(or (equal? 1 vertical) (equal? 2 vertical)) vertical]
    [(or (equal? 1 d1) (equal? 2 d1)) d1]
    [(or (equal? 1 d2) (equal? 2 d2)) d2]
    [(equal? (check-tie) #t) -1];;si hay empate retorna -1
    [else 0]));;0 no gana nadie
    

(define (check-4-vertical board)
  (define player 0); si gana p1->1,p2->2, sino 0
  (define pos 0)
  (for (
        [j (in-range 7)]);;iteramos primero columnas
    (for (
        [i (in-range 3)]);;luego iteramos filas
      (set! pos (+ (* i 7) j))
      (when (and
             (or (= (vector-ref board pos) 1) (= (vector-ref board pos) 2))
             (= (vector-ref board pos) (vector-ref board (+ 7 pos)))
             (= (vector-ref board (+ 7 pos)) (vector-ref board (+ 14 pos)))
             (= (vector-ref board (+ 14 pos)) (vector-ref board (+ 21 pos))))
        (set! player (vector-ref board pos)))))
  player)


(define (check-4-main-diagonal board) ; \
  (define player 0); si gana p1->1,p2->2, sino 0
  (define pos 0)
  (for (
        [j (in-range 4)]);;iteramos primero columnas
    (for (
        [i (in-range 3)]);;luego iteramos filas
      (set! pos (+ (* i 7) j))
      (when (and
             (or (= (vector-ref board pos) 1) (= (vector-ref board pos) 2))
             (= (vector-ref board pos) (vector-ref board (+ 8 pos)))
             (= (vector-ref board (+ 8 pos)) (vector-ref board (+ 16 pos)))
             (= (vector-ref board (+ 16 pos)) (vector-ref board (+ 24 pos))))
        (set! player (vector-ref board pos)))))
  player)

(define (check-4-second-diagonal board) ; /
  (define player 0); si gana p1->1,p2->2, sino 0
  (define pos 0)
  (for (
        [j (in-range 3 7)]);;iteramos primero columnas
    (for (
        [i (in-range 3)]);;luego iteramos filas
      (set! pos (+ (* i 7) j))
      (when (and
             (or (= (vector-ref board pos) 1) (= (vector-ref board pos) 2))
             (= (vector-ref board pos) (vector-ref board (+ 6 pos)))
             (= (vector-ref board (+ 6 pos)) (vector-ref board (+ 12 pos)))
             (= (vector-ref board (+ 12 pos)) (vector-ref board (+ 18 pos))))
        (set! player (vector-ref board pos)))))
  player)


(define (print-board board)
  (define pos 0)
    (for (
        [i '(5 4 3 2 1 0)]);;iteramos primero columnas
    (for (
        [j (in-range 7)]);;luego iteramos filas
      (set! pos (+ (* i 7) j))
      (printf " ~a " (vector-ref board pos)))(printf "\n")))

(game-loop)
