;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname TP1-ColomboNovoa) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
(require racket/bool)

; PREGUNTAS DE TEORÍA

; 1. ¿Y con el bit en 0 qué hacemos?

; Podríamos usarlo como bit de paridad de la lista de bits de paridad para en algunos
; casos poder determinar si llegaron corruptos y descartar el mensaje,
; o hasta tal vez intentar reconstruir el bloque completo, contrastando combinaciones
; de posibles bits de paridad con el mensaje.


; 2. Intente convencerse de que los 4 bits de paridad cubren todo el mensaje.
; Calcule qué conjunto de bits de paridad deben fallar para exponer cada bit fallido en el mensaje.

; Para determinar cuáles bits de paridad son los que ayudan a exponer el error en cada bit del mensaje,
; necesitamos transformar la posición de ese bit a binario


; 3. Convierta los naturales 20, 32 y 15 para familiarizarse con el algoritmo.


; 4. Las funciones de racket remainder y quotient que devuelven el resto y el cociente de hacer
; una división respectivamente pueden ser útiles.


; 5. Convierta los binarios 10100, 100000 y 1111 a números naturales para familiarizarse con el algoritmo.


; 6. ¿Quién protege a quien protege?


; REPRESENTACIÓN DE DATOS.

; Representamos mensajes o bloques de información con listas de ceros y unos.
; Tipo de dato Bit: Natural que solo puede ser 0 o 1.
; 0 = (sub1 1)
; 1 = (add1 0)
; Mensaje: lista de Bits, representa un mensaje transmitido en sistema binario de una computadora a otra.
; Binario: Lista de Bits, representa un número natural en sistema binario con sus cifras en orden
; invertido (desde la cifra menos significativa hasta la más significativa). 0 = '().

; Constantes para casos de prueba
(define MENSAJE1 (list 0 0 1 1 0 0 0 1 1 1 0))
(define MENSAJE2 (list 1 0 1 0 0 1 0 1 0 0 1))
(define MENSAJE3 (list 1 0 1 0 0 1 0 1 0 0 0))


; FUNCIONES


; EJERCICIO 1

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


; EJERCICIO 2

; natural->binario : Natural -> Binario
; Toma un Natural y lo devuelve transformado a Binario
(define (natural->binario n)
  (cond [(zero? n) '()]
        [else (cons (modulo n 2) (natural->binario (quotient n 2)))]
  )
)

; Definimos 0 como lista vacía para que no se agregue un 0 extra al final de los
; numeros distintos de 0 y para poder usar empty como caso base cuando operemos sobre el tipo Binario.

(check-expect (natural->binario 0) '())
(check-expect (natural->binario 1) (list 1))
(check-expect (natural->binario 3) (list 1 1))
(check-expect (natural->binario 5) (list 1 0 1))
(check-expect (natural->binario 6) (list 0 1 1))
(check-expect (natural->binario 34) (list 0 1 0 0 0 1))


; EJERCICIO 3

; binario->natural : Binario -> Natural
; Toma un Binario y lo devuelve transformado a Natural
(define (binario->natural b)
  (local
    (
     (define (br->nat br)
       (cond [(empty? br) 0]
             [else (+ (* (first br) (expt 2 (length (rest br)))) (br->nat (rest br)))]
       )
     )
    )
    (br->nat (reverse b))
  )
)

(check-expect (binario->natural (list 1 1)) 3)
(check-expect (binario->natural (list 1 0 1)) 5)
(check-expect (binario->natural (list 0 1 1)) 6)
(check-expect (binario->natural (list 0 1 0 0 0 1)) 34)


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