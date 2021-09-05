#lang racket


;función aplha beta, se encarga de ejecutar el algoritmo min-max con poda

(define (alphabeta node depth alpha beta maximizing)
  (cond
    [(or (= 1 (terminal node) ) (= depth 10)) "llamar a heuristica"]
    [ maximizing (maxvalue node depth  alpha beta maximizing ) ] ; maximizando la jugada
    [else(alphabeta node depth  alpha beta maximizing )] ; si viene con false entonces minimiza



    ) )

(define (terminal node) ; tengo dudas sobre cual sería un nodo terminal basado en nuestra implementación,
  (cond                 ;  sería como que ya no tenga más listas o como ? 
    [(= node 1) 1]
    [else 0]))

(define (maxvalue node depth value alpha beta)
  (define col 7); get-col devuelve las posibles jugadas
  (for (i (in-range 6)) ; por cada posible jugada hace el min max
    #:break (>= alpha beta) ;se corta esta hoja o se puede retornar null con un cond
    cond(
         [(= i 6) ]
         [else(max alpha (max val (alphabeta node (- depth 1) alpha beta #f )) )  ]
         )
         
     ; se debería enviar el hijo de el node

    )   
  
  ) ;posiciones validas


(define (minvalue node depth value alpha beta)
  (define col get-col); get-col devuelve las posibles jugadas
  (for (i col) ; por cada posible jugada hace el min max
    #:break (<= alpha beta) ;se corta esta hoja o se puede retornar null con un cond
    (max alpha (max val (alphabeta node (- depth 1) alpha beta #t )) )  ; se debería enviar el hijo de el node

    )   
  
  ) ;posiciones validas

;(alphabeta 1 0 -1 +2 true)
;(alphabeta 2 0 -1 +2 #t)

