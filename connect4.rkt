#lang racket

(require graphics/graphics) 
(require racket/gui/base)
(define v (make-vector 42))
(define celds (make-vector 7))
(define turn 1)
(define tokens 0)
(define game-status #t)
(define actual-color "red")
(define max_depth 4)

(open-graphics)


;;((draw-solid-ellipse connect)(make-posn 10 10) 100 100 "white")
;;((draw-solid-ellipse connect)(make-posn 10 120) 100 100 "white")
;;((draw-solid-ellipse connect)(make-posn 10 230) 100 100 "white")
;;((draw-solid-ellipse connect)(make-posn 10 340) 100 100 "white")
;;((draw-solid-ellipse connect)(make-posn 10 450) 100 100 "white")
;;((draw-solid-ellipse connect)(make-posn 10 560) 100 100 "red")

;;((draw-solid-ellipse connect)(make-posn 120 10) 100 100 "white")
;;((draw-solid-ellipse connect)(make-posn 230 10) 100 100 "white")
;;((draw-solid-ellipse connect)(make-posn 340 10) 100 100 "white")
;;((draw-solid-ellipse connect)(make-posn 450 10) 100 100 "white")
;;((draw-solid-ellipse connect)(make-posn 560 10) 100 100 "white")
;;((draw-solid-ellipse connect)(make-posn 670 10) 100 100 "white")


(define board-y #(560 560 560 560 560 560 560
                  450 450 450 450 450 450 450
                  340 340 340 340 340 340 340
                  230 230 230 230 230 230 230
                  120 120 120 120 120 120 120
                  10 10 10 10 10 10 10)) 
     
(define board-x #(10 120 230 340 450 560 670
                  10 120 230 340 450 560 670
                  10 120 230 340 450 560 670
                  10 120 230 340 450 560 670
                  10 120 230 340 450 560 670
                  10 120 230 340 450 560 670))


(define connect (open-viewport "Connect 4" 780 690))

(define (start-game)
((draw-viewport connect) "dark blue")(draw-board 0) (game-loop))

(define (game-loop) ;;pasar turno inicial
  (cond
    [(equal? #f game-status) 0]
    [else (game-loop-aux)]))

(define (posn-to-col x);;toma las coordenadas x del click en la ventana, utilizando intervalos para elegir la columna
  (cond
    [(and (> x 0) (< x 116)) 0]
    [(and (> x 115) (< x 226)) 1]
    [(and (> x 225) (< x 336)) 2]
    [(and (> x 335) (< x 446)) 3]
    [(and (> x 445) (< x 556)) 4]
    [(and (> x 555) (< x 666)) 5]
    [(and (> x 665) (< x 776)) 6]))

(define (game-loop-aux)

  (cond
    [(equal? turn 1) (print-board v)
                     (displayln "Turno Jugador, elija columna 0-6")
                     ;;(define token (read))
                     (get-input)
                     (displayln celds)
                     (cond [(equal? (check-win) 1)(set! game-status #f) (displayln "Jugador gana")]
                           [(equal? (check-win) 2)(set! game-status #f) (displayln "IA gana")]
                           [(equal? (check-win) -1)(set! game-status #f) (displayln "Empate")]
                           [else (change-turn) (change-color) (game-loop)])]
    [else
           (print-board v)
           (displayln "Turno IA, elija columna 0-6")
           ;;(define token (read))       
           ;;(check-valid-move token)
           ;;(get-input)
           (sleep/yield 2)
           ;;(define move )
           (check-valid-move (best-IA-move v turn celds))
           ;;(update-game-board move);;AGREGAR MINIMAX
           (cond [(equal? (check-win) 1)(set! game-status #f) (displayln "Jugador gana")]
                    [(equal? (check-win) 2)(set! game-status #f) (displayln "IA gana")]
                    [(equal? (check-win) -1)(set! game-status #f) (displayln "Empate")]
                    [else (change-turn)(change-color)(game-loop)])]))


(define (get-input)
  (get-mouse-click connect)
  (define move (posn-to-col (posn-x (query-mouse-posn connect))))
  (cond
    [(equal? (check-valid-move move)  #t) #t];;Si no es una columna valida, se encicla hasta elegir una valida, que permita colocar fichas
    [else (get-input)]))
 
(define (update-game-board col);;actualiza la UI
  (define position (+ (* 7 (vector-ref celds col)) col));; se convierte la columna en la posicion del vector del tablero
  (vector-set! v  position turn);;se actualiza el TABLERO GLOBAL, cuidado!!!
  ((draw-solid-ellipse connect);;se dibuja la ficha en el tablero
      (make-posn (vector-ref board-x position) (vector-ref board-y position)) 100 100 actual-color))
   



(define (check-valid-move col)
  (cond
   [(> (vector-ref celds col) 5) #f] ;;retorna falso si la columna ya esta llena de fichas
   [else (update-game-board col)
         (vector-set! celds col (add1 (vector-ref celds col)))
         (set! tokens (add1 tokens)) #t])) ;;retorna true si el movimiento es valido y actualiza el tope de la columna del juego



(define (change-turn);;cambiar el turno 1: IA 2: jugador
  (if (equal? turn 1)
      (set! turn 2)
      (set! turn 1)))



(define (change-color);;cambiar el turno 1: IA 2: jugador
  (if (equal? actual-color "red")
      (set! actual-color "yellow")
      (set! actual-color "red")))


;;Para contar las fichas de un tipo iguales en los grupos de 4, ya sea horizontal, vertical o diagonales

(define (count-matches word words);;word es la cadena a buscar en la lista(words)
  (cond[(null? words) 0];;si la lista esta vacia se retorna 0
       [(equal? word (first words)) (+ 1 (count-matches word (rest words)))];;se compara el primer elemento de la lista con word y se llama la recursion cortando el primer elemento
       [else (count-matches word (rest words))];;se corta la lista sin sumar a la pila,
   ))



(define (score-IA-move board token)
  (define score 0)
  ;;(main-column board turn))
  ;;(sleep/yield 1)
  (define pos 0)
    (for ( ;; horizontal check
        [i (in-range 6)])
    (for (
        [j (in-range 4)])
      (define t (list (vector-ref board pos) (vector-ref board (+ 1 pos)) (vector-ref board (+ 2 pos)) (vector-ref board (+ 3 pos))))
      (set! pos (+ (* i 7) j))
      (cond
        [(equal? (count-matches token t) 4) (set! score (+ 1000 score))]
        [(and (equal? (count-matches token t) 3) (equal? (count-matches 0 t) 1)) (set! score (+ 100 score))]
        [(and (equal? (count-matches token t) 1) (equal? (count-matches 1 t) 3)) (set! score (+ 500 score))]
        )
     ))
  (for ( ;; vertical check
        [j (in-range 7)]);;iteramos primero columnas
    (for (
        [i (in-range 3)]);;luego iteramos filas
      (set! pos (+ (* i 7) j))
      (define t (list (vector-ref board pos) (vector-ref board (+ 7 pos)) (vector-ref board (+ 14 pos)) (vector-ref board (+ 21 pos))))
      (cond
        [(equal? (count-matches token t) 4) (set! score (+ 1000 score))]
        [(and (equal? (count-matches token t) 3) (equal? (count-matches 0 t) 1)) (set! score (+ 100 score))]
        [(and (equal? (count-matches token t) 1) (equal? (count-matches 1 t) 3)) (set! score (+ 500 score))]
        )
      ))
  (for ( ;; diagonal check
        [j (in-range 4)]);;iteramos primero columnas
    (for (
        [i (in-range 3)]);;luego iteramos filas
      (set! pos (+ (* i 7) j))
            (define t (list (vector-ref board pos) (vector-ref board (+ 8 pos)) (vector-ref board (+ 16 pos)) (vector-ref board (+ 24 pos))))
            (cond
        [(equal? (count-matches token t) 4) (set! score (+ 1000 score))]
        [(and (equal? (count-matches token t) 3) (equal? (count-matches 0 t) 1)) (set! score (+ 100 score))]
        [(and (equal? (count-matches token t) 1) (equal? (count-matches 1 t) 3)) (set! score (+ 500 score))]
        )
      ))
  
    (for (
        [j (in-range 3 7)]);;iteramos primero columnas
    (for (
        [i (in-range 3)]);;luego iteramos filas
      (set! pos (+ (* i 7) j))
      (define t (list (vector-ref board pos) (vector-ref board (+ 6 pos)) (vector-ref board (+ 12 pos)) (vector-ref board (+ 18 pos))))
      (cond
        [(equal? (count-matches token t) 4) (set! score (+ 1000 score))]
        [(and (equal? (count-matches token t) 3) (equal? (count-matches 0 t) 1)) (set! score (+ 100 score))]
        [(and (equal? (count-matches token t) 1) (equal? (count-matches 1 t) 3)) (set! score (+ 500 score))]
        )
      ))
  ;;(displayln "Score")
  ;;(displayln score)
  ;;(sleep/yield 3)
  score)

(define (best-IA-move board token done-moves);;; para un tablero en especifico, se generan los posibles movimientos, board=42, done moves = 7, 
  (define temp-done-moves (vector-copy done-moves));;USAR COPIAS DEL TABLERO Y VECTORES
  (define score 0)
  (define temp-score 0)
  (define temp-col (random 0 7))
  (define temp-board (vector-copy board));;tablero temporal!!!!!!
  (set! temp-col  (minmax temp-board 0 -inf.f  +inf.f  1 temp-col temp-done-moves))
  ;; (minmax board depth alpha beta maximizingPlayer col done-moves)
    
  temp-col)
      
   
;;;PENDIENTE: agregar la variable temporal tokens como parametro y copiarla, para definir empates en los nodos terminales
(define (check-tie tmp_tokens)
  (cond
    [(equal?  tmp_tokens 42) #t]
    [else #f]))

   
;;;Funciones de GANAR

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
    [(equal? (check-tie tokens) #t) -1];;si hay empate retorna -1
    [else 0]));;0 no gana nadie

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

(define (main-column board turn);;si la IA es 1, las columnas impares tienen mayor puntaje, si la IA 2, las pares
  (define score 0)
  (cond[(and (equal? (vector-ref board 3) turn) (equal? turn 1) ) (set! score (+ score 2)) ]
       [(and (equal? (vector-ref board 3) turn) (equal? turn 2) ) (set! score (+ score 1)) ])
  
    (cond[(and (equal? (vector-ref board 10) turn) (equal? turn 2) ) (set! score (+ score 2)) ]
       [(and (equal? (vector-ref board 10) turn) (equal? turn 1) ) (set! score (+ score 1)) ])
  
    (cond[(and (equal? (vector-ref board 17) turn) (equal? turn 1) ) (set! score (+ score 2)) ]
       [(and (equal? (vector-ref board 17) turn) (equal? turn 2) ) (set! score (+ score 1)) ])
  
    (cond[(and (equal? (vector-ref board 24) turn) (equal? turn 2) ) (set! score (+ score 2)) ]
       [(and (equal? (vector-ref board 24) turn) (equal? turn 1) ) (set! score (+ score 1)) ])
  
    (cond[(and (equal? (vector-ref board 31) turn) (equal? turn 1) ) (set! score (+ score 2)) ]
       [(and (equal? (vector-ref board 31) turn) (equal? turn 2) ) (set! score (+ score 1)) ])
  
    (cond[(and (equal? (vector-ref board 38) turn) (equal? turn 2) ) (set! score (+ score 2)) ]
       [(and (equal? (vector-ref board 38) turn) (equal? turn 1) ) (set! score (+ score 1)) ])
    score)

(define (draw-board indice)
  (cond
    [(equal? indice 42)]
    [((draw-solid-ellipse connect)
      (make-posn (vector-ref board-x indice) (vector-ref board-y indice)) 100 100 "white")
     (draw-board (add1 indice))
]))





;; min max with poda



(define (minmax board depth alpha beta maximizingPlayer col done-moves)
  (define tmp_done_moves (vector-copy done-moves ))
  (define status (check-win-IA board done-moves)) ;-1 =  empate, 1=si gana jugador, 2=gana IA, 0=si no hay gane
  (cond
    [(or (= status 1) (= status -1) (= status 2) (= depth max_depth)) (aux_score_move board tokens status)] ;;hacer copia de tokens como con los vectores
    [ (= maximizingPlayer 1) (maximizing board depth alpha beta maximizingPlayer tmp_done_moves col)]
    [else (minimizing board depth alpha beta maximizingPlayer  tmp_done_moves col)]
    
    )
  )
(define (minimizing board depth alpha beta maximizingPlayer done-moves col)
   (define tmp_col_score (make-vector 2))
  (vector-set! tmp_col_score 0 col )
  (vector-set! tmp_col_score 1 -10000000)
  (define tmp_beta beta)
    (for (
           
        [i (in-range 7)]);;para cada columna si la ficha es valida
      (cond
        [(< (vector-ref done-moves i) 6)
           (define temp-board (vector-copy board));;tablero temporal!!!!!!
           (define tmp-done-moves  (vector-copy done-moves))
           (define position (+ (* 7 (vector-ref done-moves i)) i));;posicion en el tablero temporal
           (vector-set! temp-board position 2);;se actualiza el tablero temporal
           (vector-set! tmp-done-moves i (+ 1 (vector-ref tmp-done-moves i)))
           (define new-score (min (vector-ref tmp_col_score 1 ) (minmax  temp-board (+ 1 depth) alpha  tmp_beta 1 (vector-ref tmp_col_score 0)  tmp-done-moves )))
           (cond
             [ (> new-score (vector-ref tmp_col_score 1 )) (set! tmp_col_score #(i new-score))])
           (set!  tmp_beta (min beta  (vector-ref tmp_col_score 1 )) )
            '#:break (>= alpha  tmp_beta)

           ]
        )
      )

  (vector-ref tmp_col_score 0 )
  )
   

(define (maximizing board depth alpha beta maximizingPlayer done-moves col)
  (define tmp_col_score (make-vector 2))
  (vector-set! tmp_col_score 0 col )
  (vector-set! tmp_col_score 1 -10000000)
  (define tmp_col col)
  (define tmp_alpha alpha)
    (for (
           
        [i (in-range 7)]);;para cada columna si la ficha es valida
      (cond
        [(< (vector-ref done-moves i) 6)
           (define temp-board (vector-copy board));;tablero temporal!!!!!!
           (define tmp-done-moves  (vector-copy done-moves))
           (define position (+ (* 7 (vector-ref done-moves i)) i));;posicion en el tablero temporal
           (vector-set! temp-board position 2);;se actualiza el tablero temporal
           (vector-set! tmp-done-moves i (+ 1 (vector-ref tmp-done-moves i)))
           (displayln tmp_col_score )
           (define new-score (max  (vector-ref tmp_col_score 1 ) (minmax  temp-board (+ 1 depth) tmp_alpha beta 0 (vector-ref tmp_col_score 0) tmp-done-moves )))
           (cond
             [ (> new-score (vector-ref tmp_col_score 1 )) (vector-set! tmp_col_score 0 i ) (vector-set! tmp_col_score 1 new-score)])
           (set! tmp_alpha (max alpha (vector-ref tmp_col_score 1 )) )
            (cond
             [(= col -1) (set! tmp_col i)])
            '#:break (>= tmp_alpha beta)

           ]
        )
      )
 (vector-ref tmp_col_score 0 )   
)

(define (aux_score_move board tokens status)
     (cond
       [( = status 1) -1000000000]
       [( = status 2) 1000000000]
       [( = status -1) 0]
       [else (score-IA-move board 2)]
       )
      )   ;; (score-IA-move board token)

(define (check-win-IA board tokens);;empate: -1, jugador 1, IA: 2
  (define horizontal (check-4-horizontal board))
  (define vertical (check-4-vertical board))
  (define d1 (check-4-main-diagonal board))
  (define d2 (check-4-second-diagonal board))
  (cond
    [(or (equal? 1 horizontal) (equal? 2 horizontal)) horizontal]
    [(or (equal? 1 vertical) (equal? 2 vertical)) vertical]
    [(or (equal? 1 d1) (equal? 2 d1)) d1]
    [(or (equal? 1 d2) (equal? 2 d2)) d2]
    [(equal? (check-tie tokens) #t) -1];;si hay empate retorna -1
    [else 0]));;0 no gana nadie

(start-game)
