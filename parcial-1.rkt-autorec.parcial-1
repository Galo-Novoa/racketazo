;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname parcial-1) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; EJERCICIO 1

; Representamos cada color como una estructura de nombre color que toma los valores de rojo, verde y azul sumados
; suma-rojo : Color -> Color

(define ROJO-EXCEDENTE (- 255 15))
(define (suma-rojo c)
  (if (color? c)
    (make-color
      (if (> (color-red c) ROJO-EXCEDENTE) (- (color-red c) ROJO-EXCEDENTE) (+ (color-red c) 15))
      (color-green c) (color-blue c)
    )
  (make-color 0 0 0)
  )
)

(check-expect (suma-rojo (make-color 205 92 92)) (make-color 220 92 92))
(check-expect (suma-rojo (make-color 255 30 50)) (make-color 15 30 50))
(check-expect (suma-rojo (make-color 250 245 50)) (make-color 10 245 50))


; EJERCICIO 2

; DISEÑO DE DATOS: ESTADO
; El estado en este caso es una estructura que contiene una
; Imagen (la escena, que comienza vacia y a la que se le van a ir colocando las flores)
; , el contador de segundos y el color del que se van a pintar las flores.
 
; DEFINICIÓN DE CONSTANTES
(define ALTO 500)
(define ANCHO 500)
(define FONDO (empty-scene ANCHO ALTO)) ; escena vacía
(define TIEMPO-INICIAL 0)
(define COLOR-INICIAL (make-color 205 92 92))
(define DOS-SEGUNDOS 56)
 
; Estado inicial
(define-struct app (mapa tiempo colorn))
(define INICIAL (make-app FONDO TIEMPO-INICIAL COLOR-INICIAL))
(define (dibujar-flor colorn)
  (pulled-regular-polygon 40 3 1.8 30 "solid" colorn)
)

; coord-y-espejo : Number -> Number
(define (coord-y-espejo n)
  (if (and (>= n 0) (<= n ALTO))
      (- ALTO n)
      (if (< n 0) 0 ALTO))
)

(check-expect (coord-y-espejo 100) 400)
(check-expect (coord-y-espejo 400) 100)
(check-expect (coord-y-espejo 50) 450)
(check-expect (coord-y-espejo 450) 50)
(check-expect (coord-y-espejo 750) 500)
(check-expect (coord-y-espejo -80) 0)
 
; ——— Función asociada a la cláusula to-draw de la expresión big-bang
; mostrar-mapa : App -> Image
(define (mostrar-mapa appn) (app-mapa appn))

 
; ——— Función asociada al manejador del mouse
; manejador-mouse : App Number Number String -> App
(define (manejador-mouse appn x y evento)
  (make-app
    (if (string=? evento "button-down")
      (place-image
        (dibujar-flor (app-colorn appn)) x (coord-y-espejo y)
        (place-image (dibujar-flor (app-colorn appn)) x y (app-mapa appn)))
      (app-mapa appn)
    )
    (app-tiempo appn)
    (app-colorn appn)
  )
)

; ————— Función asociada al manejador del reloj
; manejador-tick : App -> App
(define (manejador-tick appn)
  (if (= (modulo (app-tiempo appn) DOS-SEGUNDOS) 0)
    (make-app
      (app-mapa appn)
      (+ (app-tiempo appn) 1)
      (suma-rojo (app-colorn appn))
    )
    (make-app
      (app-mapa appn)
      (+ (app-tiempo appn) 1)
      (app-colorn appn)
    )
  )
)
 
; — Expresión big-bang —
(big-bang INICIAL
  [on-tick manejador-tick]
  [to-draw mostrar-mapa]
  [on-mouse manejador-mouse]
)