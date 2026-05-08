;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname entrega-parcial-galo-novoa) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; EJERCICIO 1

; Representamos cada color como una estructura de nombre color que toma los valores de rojo, verde y azul sumados
; suma-rojo : Color -> Color
(define (suma-rojo color)
  (if (color? color) (
      (make-color
        (if (> color-red 235) (255 - color-red) (+ color-red 15))
        color-green color-blue
      )
    )
  (make-color 0 0 0)
  )
)

(define (resta-color c) (- c-red 25))
; No me acuerdo la sintaxis para acceder a la propiedad de una estructura cuando es argumento de una funcion
; , asi que todas las funciones en las que haya que hacerlo van a dar error, pero espero que se entienda mi logica
; en este ejemplo (resta-color) c seria el color que recibe de argumento y c-red la propiedad red del color c

(check-expect (suma-rojo (make-color 205 92 92)) (make-color 220 92 92))
(check-expect (suma-rojo (make-color 255 30 50)) (make-color 15 30 50))
(check-expect (suma-rojo (make-color 250 245 50)) (make-color 10 245 50))


; EJERCICIO 2

; coord-y-espejo : Number -> Number
(define (coord-y-espejo n) (if (and (>= n 0) (<= n ALTO)) (- ALTO n) (if (< n 0) 0 ALTO)))

(check-expect (coord-y-espejo 100) 400)
(check-expect (coord-y-espejo 400) 100)
(check-expect (coord-y-espejo 50) 450)
(check-expect (coord-y-espejo 450) 50)
(check-expect (coord-y-espejo 750) 500)
(check-expect (coord-y-espejo -80) 0)

; DISEÑO DE DATOS: ESTADO
; El estado en este caso es una estructura que contiene una
; Imagen (la escena, que comienza vacia y a la que se le van a ir colocando las flores)
; , el contador de segundos y el color del que se van a pintar las flores.
 
; DEFINICIÓN DE CONSTANTES
(define ALTO 500)
(define ANCHO 500)
(define FONDO (empty-scene ANCHO ALTO)) ; escena vacía
(define FLOR (pulled-regular-polygon 40 3 1.8 30 "solid" "Indian Red"))
(define TIEMPO-INICIAL 0)
(define COLOR-INICIAL (make-color 205 92 92))
 
; Estado inicial
(define-struct app (mapa tiempo colorn))
(define INICIAL (make-app (FONDO TIEMPO-INICIAL COLOR-INICIAL)))
 
; ——— Función asociada a la cláusula to-draw de la expresión big-bang
; mostrar-mapa : App -> Image
(define (mostrar-mapa appn) appn-mapa)

 
; ——— Función asociada al manejador del mouse
; manejador-mouse : App Number Number String -> App
(define (manejador-mouse appn x y evento)
  (place-image FLOR x (coord-y-espejo y) (place-image FLOR x y FONDO))
)

; ————— Función asociada al manejador del reloj
; manejador-tick : App -> App
(define (manejador-tick appn) (cond
  [(= (modulo appn-tiempo 2) 0) (make-app appn-mapa (+ appn-tiempo 1) (suma-rojo appn-color))]
  [else (make-app appn-mapa (+ appn-tiempo 1) appn-color)])
)
 
; — Expresión big-bang —
(big-bang INICIAL
  [on-tick manejador-tick]
  [to-draw mostrar-mapa]
  [on-mouse manejador-mouse]
)
