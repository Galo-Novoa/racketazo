;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname practica-5a) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

; buscar-contacto : Contactos -> Bool
(define (buscar-contacto l c)
  (if (empty? l) #f
      (if
        (string=? (first l) c)
        #t
        (buscar-contacto (rest l) c)
      )
  )
)

(define (contiene-Marcos? l) (cond [(empty? l) #false]
                                   [(cons? l) (if (string=? (first l) "Marcos")
                                                  #true
                                                  (contiene-Marcos? (rest l)))]))

(contiene-Marcos? (cons "Marcos" (cons "C" '())))

(contiene-Marcos? (cons "A" (cons "Marcos" (cons "C" '()))))

(contiene-Marcos? (cons "A" (cons "B" (cons "C" '()))))

(cons 45 (cons 23 (cons 9 '())))

; suma-montos : Listof Number -> Number
(define (suma-montos l)
  (cond
    [(empty? l) 0]
    [(cons? l)
       (+
                (first l)
                (suma-montos (rest l))
       )
    ]
  )
)

; pos? : Listof Number -> Bool
(define (pos? l)
  (cond [(empty? l) #t]
        [(cons? l)
          (if (>= (first l) 0)
              (pos? (rest l))
              #f
          )
        ]
   )
)

; checked-sum : Listof Number -> Number
(define (checked-sum l)
  (if (pos? l) (suma-montos l) "No es una lista de positivos")
)

; cantidad ; Listof Number -> Listof Number
(define (cantidad l)
  (cond [(empty? l) 0]
        [else (+ 1 (cantidad (rest l)))]
  )
)

; pares : Listof Number -> Listof Number
(define (pares l)
  (cond
    [(empty? l) '()]
    [(cons? l)
      (if (even? (first l))
          (cons (first l) (pares (rest l)))
          (pares (rest l))
      )
    ]
  )
)

; cortas : Listof String -> Listof String
(define (cortas l)
  (cond [(empty? l) '()]
        [(>= (string-length (first l)) 5) (cortas (rest l))]
        [else (cons (first l) (cortas (rest l)))]
  )
)

; mod : Posn -> Number
(define (mod pn)
  (sqrt (+ (sqr (posn-x pn)) (sqr (posn-y pn))))
)

(define MAX 5)

; cerca : Listof Posn -> Listof Posn
(define (cerca l)
  (cond [(empty? l) '()]
        [(< (mod (first l)) MAX) (cons (first l) (cerca (rest l)))]
        [else (cerca (rest l))]
  )
)

; maximo : Listof Number -> Number
(define (maximo l)
  (cond [(empty? l) 0]
        [(> (first l) (maximo (rest l))) (first l)]
        [else (maximo (rest l))]
  )
)

; Ejercicio 30

; constantes
(define ESTRELLA (star 40 "solid" "gold"))
(define ANCHO 1000)
(define ALTO 500)
(define FONDO (empty-scene ANCHO ALTO))

; to-draw
(define (interpretar-e s)
  (cond [(empty? s) FONDO]
        [else
         (place-image
          ESTRELLA
          (posn-x (first s))
          (posn-y (first s))
          (interpretar-e (rest s))
          )
        ]
  )
)

; on-mouse
(define (nueva-estrella s x y e)
  (cond [(string=? e "button-down") (cons (make-posn x y) s)]
        [else s]
  )
)

; on-key
(define (borrar-estrella s e)
  (cond [(empty? s) '()]
        [(key=? e "e") '()]
        [(key=? e "d") (rest s)]
        [else s]
  )
)

; big-bang
(define (programa-estrellas go)
   (big-bang '()
    [to-draw interpretar-e]
    [on-mouse nueva-estrella]
    [on-key borrar-estrella]
   )
)


; Ejercicio 31


; constantes y auxiliares
(define-struct turno (ant act))
(define INICIAL-S (make-turno empty empty))
(define CENTRO-X (/ ANCHO 2))
(define CENTRO-Y (/ ALTO 2))
(define GAME-OVER-FLAG '("0"))
(define (es-flechita? e)
  (or
    (string=? e "up")
    (string=? e "down")
    (string=? e "left")
    (string=? e "right")
  )
)

(define (jugador-1? ant) (= (modulo (length ant) 2) 0))

; to-draw
(define (interpretar-s s)
  (cond [
         (jugador-1? (turno-ant s))
         (place-image (text "Jugador 1" 32 "red") CENTRO-X CENTRO-Y FONDO)
        ]
        [else (place-image (text "Jugador 2" 32 "blue") CENTRO-X CENTRO-Y FONDO)]
  )
)

; on-key y auxiliares

(define (pasar-turno s e)
  (cond [(not (es-flechita? e)) (make-turno (turno-ant s) (turno-act s))]
        [(<= (length (turno-act s)) (length (turno-ant s))) (make-turno (turno-ant s) (cons e (turno-act s)))]
        [else (make-turno (turno-ant s) (turno-act s))]
  )
)

; on-tick
(define (planck s)
  (cond [(> (length (turno-act s)) (length (turno-ant s)))
         (cond [(equal? (turno-ant s) (rest (turno-act s))) (make-turno (turno-act s) '())]
               [else (make-turno (turno-ant s) GAME-OVER-FLAG)])
        ]
        [else (make-turno (turno-ant s) (turno-act s))]
  )
)

; stop-when
(define (game-over? s) (equal? (turno-act s) GAME-OVER-FLAG))

(define (print-game-over s)
  (cond [(jugador-1? (turno-ant s)) (place-image (text "Ganador: Jugador 2" 32 "blue") CENTRO-X CENTRO-Y FONDO)]
        [else (place-image (text "Ganador: Jugador 1" 32 "red") CENTRO-X CENTRO-Y FONDO)]
  )
)

; big-bang
(define (simon-says go)
   (big-bang INICIAL-S
    [to-draw interpretar-s]
    [on-key pasar-turno]
    [on-tick planck]
    [stop-when game-over? print-game-over]
   )
)