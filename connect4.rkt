#lang racket

(require graphics/graphics) 
(require racket/gui/base)
;;el tablero se representa con un vector de 42 elementos,la posicion se determina con (fila*7)+columna
(define v (make-vector 42))
;;define  turno inicial 1: jugador 2: inicia la IA
(define turn 1)
;;vector del tope de las columnas
(define celds (make-vector 7))
;;cantidad de piezas colocadas en el tablero
(define tokens 0)
;;definicion del status del juego
(define game-status #t)
;;color de la pieza inicial en el tablero
(define actual-color "red")
;;definicion de la profundidad del minimax
(define max-depth 5)
;;pieza que representa la IA
(define IA-token 2)
;;pieza que representa el jugador
(define player-token 1) 



;;--------------------------INTERFAZ GRAFICA--------------------------------------;;

;;Inicializa la rutinas de la biblioteca de graficos
(open-graphics)


;;Definicion del vector que contiene las coordenadas y de los circulos de la interfaz

(define board-y #(560 560 560 560 560 560 560
                  450 450 450 450 450 450 450
                  340 340 340 340 340 340 340
                  230 230 230 230 230 230 230
                  120 120 120 120 120 120 120
                  10 10 10 10 10 10 10))

;;Definicion del vector que contiene las coordenadas x de los circulos de la interfaz
     
(define board-x #(10 120 230 340 450 560 670
                  10 120 230 340 450 560 670
                  10 120 230 340 450 560 670
                  10 120 230 340 450 560 670
                  10 120 230 340 450 560 670
                  10 120 230 340 450 560 670))

;;Crea la ventana del juego con las dimensiones especificadas

(define connect (open-viewport "Connect 4" 780 690))

;;Se encarga de pintar la ventana de color azul y llama la funcion que dibuja los circulos del tablero

(define (start-game)
((draw-viewport connect) "dark blue")(draw-board 0) (game-loop))

;;Entrada: Un valor entero que representa el indice de un vector
;;Salida: El tablero con los circulos para las piezas del juego
(define (draw-board indice)
  (cond
    [(equal? indice 42)]
    [((draw-solid-ellipse connect)
      (make-posn (vector-ref board-x indice) (vector-ref board-y indice)) 100 100 "white")
     (draw-board (add1 indice))
]))

;;Entrada: Int que representa la coordenada X del tablero
;;Salida: El valor de la columna
;;Toma las coordenadas x del click en la ventana, utilizando intervalos para elegir la columna, 7 columnas en total
(define (posn-to-col x)
  (cond
    [(and (> x 0) (< x 116)) 0]
    [(and (> x 115) (< x 226)) 1]
    [(and (> x 225) (< x 336)) 2]
    [(and (> x 335) (< x 446)) 3]
    [(and (> x 445) (< x 556)) 4]
    [(and (> x 555) (< x 666)) 5]
    [(and (> x 665) (< x 776)) 6]))

;;Entrada: Las coordenadas (x,y) del click efectuado en la ventana
(define (get-input)
  (get-mouse-click connect)
  (define move (posn-to-col (posn-x (query-mouse-posn connect))))
  (cond
    [(equal? (check-valid-move move)  #t) #t];;Si no es una columna valida, se encicla hasta elegir una valida, que permita colocar fichas
    [else (get-input)]))

;;Entrada: Valor entero que representa la columna valida para efectuar un movimiento
;;Salida: Actualizacion en la ventana del tablero con la nueva pieza colocada

(define (update-game-board col)
  ;;la posicion de la pieza en el tablero se define como ()
  (define position (+ (* 7 (vector-ref celds col)) col))
  ;;se actualiza el tablero del juego
  (vector-set! v  position turn)
  ;;se dibuja la ficha en el tablero
  ((draw-solid-ellipse connect)
      (make-posn (vector-ref board-x position) (vector-ref board-y position)) 100 100 actual-color))


;;--------------------------CICLO DEL JUEGO--------------------------------------;;

;;Funcion principal que determina si el juego ha terminado o continua, mediante recursion

(define (game-loop) 
  (cond
    [(equal? #f game-status) 0]
    [else (game-loop-aux)]))

;;Funcion auxiliar de la recursion del ciclo del juego

(define (game-loop-aux)
  (cond
    ;;Si es 1, el turno es del juegador
    [(equal? turn 1) (print-board v)
                     (displayln "Turno Jugador, elija columna 0-6")
                     (get-input)
                     (cond [(equal? (check-win) 1)(set! game-status #f) (displayln "Jugador gana")]
                           [(equal? (check-win) 2)(set! game-status #f) (displayln "IA gana")]
                           [(equal? (check-win) -1)(set! game-status #f) (displayln "Empate")]
                           [else (change-turn) (change-color) (game-loop)])]
    [else;;sino es el turno de la IA
           (print-board v)
           (displayln "Turno IA, elija columna 0-6")
           ;;(sleep/yield 2)
           (time
            (check-valid-move (first (best-IA-move v turn celds)))  (void))
        
           (cond [(equal? (check-win) 1)(set! game-status #f) (displayln "Jugador gana")]
                    [(equal? (check-win) 2)(set! game-status #f) (displayln "IA gana")]
                    [(equal? (check-win) -1)(set! game-status #f) (displayln "Empate")]
                    [else (change-turn)(change-color)(game-loop)])]))


;;Entrada: Un entero que representa la columna del tablero
;;Salida: True si es un movimiento valido y False si es incorrecto
(define (check-valid-move col)
  (cond
   [(> (vector-ref celds col) 5) #f] ;;retorna falso si la columna ya esta llena de fichas
   [else (update-game-board col)
         (vector-set! celds col (add1 (vector-ref celds col)))
         (set! tokens (add1 tokens)) #t])) ;;retorna true si el movimiento es valido y actualiza el tope de la columna del juego


;;Entrada: No recibe parametros
;;Salida: Cambia el valor de la variable global turn
(define (change-turn);;cambiar el turno 1: IA 2: jugador
  (if (equal? turn 1)
      (set! turn 2)
      (set! turn 1)))


;;Entrada:No recibe parametros
;;Salida: Cambia el valor de la variable del color de la pieza para el tablero
(define (change-color);;cambiar el turno 1: IA 2: jugador
  (if (equal? actual-color "red")
      (set! actual-color "yellow")
      (set! actual-color "red")))


;;--------------------------PUNTAJE DEL JUEGO--------------------------------------;;


;;Entrada: Recibe una lista de enteros y un entero 0 o 1
;;Salida: Retorna 1 si existe en la lista una amenaza de piezas para el contricante, Amenazas de 1, 2 y 3 piezas
(define (tokens-score tokens token);; 
    (if (and
       (= (list-ref tokens 0) token)
       (= (list-ref tokens 1) token)
       (= (list-ref tokens 2) token)
       (= (list-ref tokens 3) token)
      ) 10 (if (and
       (= (list-ref tokens 0) token)
       (or
       (= (list-ref tokens 1) token)
       (= (list-ref tokens 1) 0))
       (or
       (= (list-ref tokens 2) token)
       (= (list-ref tokens 2) 0))
       (or
       (= (list-ref tokens 3) token)
       (= (list-ref tokens 3) 0))
      ) 1 0)))

;;Entrada: Recibe como parametro el vector del tablero del juego
;;Salida: El peso del tablero que se define como (Puntaje IA - Puntaje Jugador)  
 
(define (score-IA-move board)
  (define score-IA 0)
  (define score-player 0)
  (define pos 0)
    (for (
        [i (in-range 6)])
    (for (
        [j (in-range 4)])
      (set! pos (+ (* i 7) j))
      (define t (list (vector-ref board pos) (vector-ref board (+ 1 pos)) (vector-ref board (+ 2 pos)) (vector-ref board (+ 3 pos))))
      (set! score-IA (+ score-IA (tokens-score t IA-token)))
      (set! score-player (+ score-player (tokens-score t player-token)))))
  (for ( ;; vertical check
        [j (in-range 7)]);;iteramos primero columnas
    (for (
        [i (in-range 3)]);;luego iteramos filas
      (set! pos (+ (* i 7) j))
      (define t (list (vector-ref board pos) (vector-ref board (+ 7 pos)) (vector-ref board (+ 14 pos)) (vector-ref board (+ 21 pos))))
      (set! score-IA (+ score-IA (tokens-score t IA-token)))
      (set! score-player (+ score-player (tokens-score t player-token)))))
;;(displayln "Diagonal /") 
  (for ( ;; diagonal check
        [j (in-range 3 7)]);;iteramos primero columnas
    (for (
        [i (in-range 3)]);;luego iteramos filas
      (set! pos (+ (* i 7) j))
      (define t (list (vector-ref board pos) (vector-ref board (+ 6 pos)) (vector-ref board (+ 12 pos)) (vector-ref board (+ 18 pos))))
      (set! score-IA (+ score-IA (tokens-score t IA-token)))
      (set! score-player (+ score-player (tokens-score t player-token)))))
  ;;(displayln "Diagonal \\") 
    (for (
        [j (in-range 4)]);;iteramos primero columnas
    (for (
        [i (in-range 3)]);;luego iteramos filas
      (set! pos (+ (* i 7) j))
      (define t (list (vector-ref board pos) (vector-ref board (+ 8 pos)) (vector-ref board (+ 16 pos)) (vector-ref board (+ 24 pos))))
      (set! score-IA (+ score-IA (tokens-score t IA-token)))
      (set! score-player (+ score-player (tokens-score t player-token)))))
  (- score-IA score-player))

;;Entrada: El tablero como vector y el total de fichas que contiene
;;Salida: El peso del tablero
(define (evaluation board total)
  (define status (check-win-IA board total))
  (cond [(equal? status 2) 10000];;puntaje maximo de la IA
        [(equal? status 1) -10000];;puntaje maximo del jugador
        [else (score-IA-move board) ]))


;;Entrada: El tablero,la pieza de la IA y el vector que determina los movimientos validos
;;Salida: Una lista con la columna con el mejor movimiento y el peso
(define (best-IA-move board token done-moves);;; para un tablero en especifico, se generan los posibles movimientos, board=42, done moves = 7,
  (define result (minimax board 0 -10000 10000 IA-token 3 done-moves tokens))
  (displayln result)
  result)
      
   
;;Entrada: El total de piezas en el juego
;;Salida: True si el juego no puede poner mas piezas y False si hay movimientos disponibles
(define (check-tie tmp_tokens)
  (cond
    [(equal?  tmp_tokens 42) #t]
    [else #f]))

;;Entrada: No recibe parametros porque utiliza el tablero global
;;Salida: Un entero que determina el estado del tablero ;empate: -1, jugador 1, IA: 2 
(define (check-win);
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

;;--------------------------CHECK VICTORIA--------------------------------------;;

;;Entrada: Recibe como parametro el vector del tablero
;;Salida: Un entero que representa el ganador del juego 1:Jugador 2:IA 0:No hay ganador
(define (check-4-horizontal board)
  (define player 0)
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

;;Entrada: Recibe como parametro el vector del tablero
;;Salida: Un entero que representa el ganador del juego 1:Jugador 2:IA 0:No hay ganador
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

;;Entrada: Recibe como parametro el vector del tablero
;;Salida: Un entero que representa el ganador del juego 1:Jugador 2:IA 0:No hay ganador
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

;;Entrada: Recibe como parametro el vector del tablero
;;Salida: Un entero que representa el ganador del juego 1:Jugador 2:IA 0:No hay ganador
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

;;Entrada: Recibe como parametro el vector del tablero
;;Salida: Imprime en la terminal el tablero 
(define (print-board board)
  (define pos 0)
    (for (
        [i '(5 4 3 2 1 0)]);;iteramos primero columnas
    (for (
        [j (in-range 7)]);;luego iteramos filas
      (set! pos (+ (* i 7) j))
      (printf " ~a " (vector-ref board pos)))(printf "\n")))



;;--------------------------MINIMAX--------------------------------------;;


;;Entrada: El tablero, la profundidad actual, alpha , beta, el jugador o IA, posicion, vector de movimientos hechos, total de movimientos
(define (minimax board depth alpha beta maximizing position done-moves total)
  (define status (check-win-IA board total)) ;-1 =  empate, 1=si gana jugador, 2=gana IA, 0=si no hay gane;; saber si es terminal o no
  (cond
    [(= depth max-depth) (list " " (evaluation board total))];;n
    [( = status 1) (list "-" -10000)];;puntaje maximo Jugador
    [( = status 2) (list "-" 10000)] ;;puntaje maximo IA
    [( = status -1) (list "-" 0)];;puntaje empate
    [(= maximizing IA-token) (maximize board depth alpha beta -10000 3 maximizing done-moves 0 total)];;se llama la funcion que maximiza el puntaje de la IA
    [(= maximizing player-token) (minimize board depth alpha beta 10000 3 maximizing done-moves 0 total)];;se llama la funcion que minimiza el puntaje del juagdor
))

;;Entrada: El vector del tablero, la profundidad actual, el valor entero de alpha y beta, el puntaje maximo acumulado, columna del puntaje maximo
;;Salida: La lista con el puntaje maximo y la columna que corresponde
(define (maximize board depth alpha beta max-score max-col maximizing done-moves col total)
  (cond
    [(equal? col 7) (list max-col max-score)];;si termino de recorrer los 7 posibles tableros en la profundidad actual retorno el puntaje maximo y la columna
    [(>= alpha beta) (list max-col max-score)];;si realizo una poda, termina la recursion y se retorna la lista del puntaje maximo
    [else
     (cond
        [(< (vector-ref done-moves col) 6);;si la columna es valida
           (define temp-board (vector-copy board))
           (define temp-done-moves  (vector-copy done-moves))
           (vector-set! temp-board (+ (* 7 (vector-ref done-moves col)) col) maximizing)
           (vector-set! temp-done-moves col (+ 1 (vector-ref temp-done-moves col)))
           ;;llamada recursiva aumentando la profundidad y el total, con la pieza del jugador para minimizar en la siguiente llamada a minimax
           (define new-score (minimax temp-board (+ 1 depth) alpha beta player-token col temp-done-moves (+ 1 total)))
           (cond [(> (second new-score) max-score)
               ;;si el puntaje nuevo es mayor que el acumulado se llama a la recursion con los valores actualizados
               (maximize board depth (max alpha (second new-score)) beta (second new-score) col maximizing done-moves (+ 1 col) total)]
                ;;sino solo se cambia el alpha por el maximo del alpha y el puntaje maximo
               [else (maximize board depth (max alpha max-score) beta max-score max-col maximizing done-moves (+ 1 col) total)])]
           ;;si no es un puntaje maximo, se llama la recursion para el siguiente tablero
           [else(maximize board depth alpha beta max-score max-col maximizing done-moves (+ 1 col) total)])]))

;;Entrada: El vector del tablero, la profundidad actual, el valor entero de alpha y beta, el puntaje minimo acumulado, columna del puntaje minimo
;;Salida: La lista con el puntaje minimo y la columna que corresponde
(define (minimize board depth alpha beta max-score max-col maximizing done-moves col total)
  (cond
    [(equal? col 7) (list max-col max-score)]
    [(>= alpha beta) (list max-col max-score)]
    [else
     (cond
        [(< (vector-ref done-moves col) 6)
           (define temp-board (vector-copy board))
           (define temp-done-moves  (vector-copy done-moves))
           (vector-set! temp-board (+ (* 7 (vector-ref done-moves col)) col) maximizing)
           (vector-set! temp-done-moves col (+ 1 (vector-ref temp-done-moves col)))
           (define new-score (minimax temp-board (+ 1 depth) alpha beta IA-token col temp-done-moves (+ 1 total)))
           (cond [(< (second new-score) max-score)
               (minimize board depth alpha (min beta (second new-score)) (second new-score) col maximizing done-moves (+ 1 col) total)]
                 [else
               (minimize board depth alpha (min beta max-score) max-score max-col maximizing done-moves (+ 1 col) total)])]
        [else(minimize board depth alpha beta max-score max-col maximizing done-moves (+ 1 col) total)]
        )]
    ))

;;Entrada: El vector del tablero y el total de piezas
;;Salida: El estado del tablero si hay un ganador, empate o no  
(define (check-win-IA board tokens)
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
