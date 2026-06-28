;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname TP1-ColomboNovoa) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
(require racket/bool)

; PREGUNTAS DE TEORÍA

; 1. Intente convencerse de que los 4 bits de paridad cubren todo el mensaje.
; Calcule qué conjunto de bits de paridad deben fallar para exponer cada bit fallido en el mensaje.

; Para determinar cuáles bits de paridad son los que ayudan a exponer el error en cada bit del mensaje,
; necesitamos transformar la posición de ese bit a binario

; Representación de datos.
; Representamos mensajes o bloques de información con listas de ceros y unos.
; Tipo de dato Bit: Natural que solo puede ser 0 o 1.
; 0 = (sub1 1)
; 1 = (add1 0)
; Mensaje: lista de Bits
; Bloque: Lista de Listas de Bits?

; Constantes para casos de prueba
(define MENSAJE1 (list 0 0 1 1 0 0 0 1 1 1 0))
(define MENSAJE2 (list 1 0 1 0 0 1 0 1 0 0 1))
(define MENSAJE3 (list 1 0 1 0 0 1 0 1 0 0 0))


; FUNCIONES

; paridad : List(Bit) -> Bit
; Toma un Mensaje, devuelve 1 si
; la cantidad de unos en el Mensaje es impar,
; y 0 si es par.
(define (paridad m)
  (cond [(empty? m) 0]
        [(= (first m) 0) (paridad (rest m))]
        [(= (paridad (rest m)) 0) 1]
        [else 0]
  )
)

; Definición con patrones de alto orden
; (define (paridad m)
;   (cond [(empty? m) 0]
;         [(even? (foldr + 0 m)) 0]
;         [else 1]
;   )
; )

(check-expect (paridad '()) 0)
(check-expect (paridad MENSAJE1) 1)
(check-expect (paridad MENSAJE2) 1)
(check-expect (paridad MENSAJE3) 0)
L,R  

; -- Esta función es parte de la plantilla dada --
; n-bits-redundancia: Number -> Number
; n-bits-redundancia recibe el largo de un mensaje y calcula
; la cantidad de bits de paridad que se requieren para cubrir
; el mensaje completo según la técnica de corrección de Hamming
(check-expect (n-bits-redundancia 1) 2)
(check-expect (n-bits-redundancia 4) 3)
(check-expect (n-bits-redundancia 11) 4)
(check-expect (n-bits-redundancia 13) 5)
(check-expect (n-bits-redundancia 26) 5)
(check-expect (n-bits-redundancia 27) 6)
(define (n-bits-redundancia n)
  (local [(define aprox (inexact->exact (ceiling (/ (log (+ n 1)) (log 2)))))]
    (if (>= (expt 2 aprox) (+ n aprox 1))
        aprox
        (+ aprox 1))))

; -- Esta función es parte de la plantilla dada --
; tamaño-bloque: Number -> Number
; tamaño-bloque recibe el largo de un mensaje y devuelve
; el tamaño de bloque que se requiere para agregarle bits
; de corrección de Hamming
(define (tamaño-bloque largo-mensaje)
  (+ largo-mensaje (n-bits-redundancia largo-mensaje) 1))

; -- Esta función es parte de la plantilla dada --
; posiciones: Number -> List(Number)
; posiciones recibe un número y devuelve una lista con números
; del 0 al n sin incluir.
(define (posiciones n)
    (range 0 n 1))

; -- Esta función es parte de la plantilla dada --
; bloque-a-bits-redundancia: Number -> Number
; bloque-a-bits-redundancia recibe el tamaño de un bloque
; con corrección de Hamming y devuelve la cantidad de bits
; de paridad que se necesitan para cubrirlo
(define (bloque-a-bits-redundancia n)
  (/ (log n) (log 2)))

(define-struct Par [p s])
; Par es un (Any, Any)
; p es la primera componente del par
; s es la segunda componente del par