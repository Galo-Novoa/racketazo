;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname TP1-ColomboNovoa) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
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
; Bit: Natural que solo puede ser 0 o 1.
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
     ; br->nat : List(Bit) -> Natural
     ; Recibe un Binario revertido (primer bit es el más significativo hasta el menor)
     ; y lo transforma en natural, es el mismo número que el original solo que con
     ; los bits ordenados al revés para facilitar la transformación.
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


; EJERCICIO 4

; uno-en-pos? : Binario Natural -> Bool
; Toma un Binario b y un Natural p, devuelve
; #true si hay un 1 en la posición p de b, #false si no.
(define (uno-en-pos? b p)
  (cond [(empty? b) #f]
        [(zero? p) (= 1 (first b))]
        [else (uno-en-pos? (rest b) (sub1 p))]
  )
)

(check-expect (uno-en-pos? (list 1 0 0) 0) #true)
(check-expect (uno-en-pos? (list 0 1 0) 1) #true)
(check-expect (uno-en-pos? (list 0 0 1) 2) #true)
(check-expect (uno-en-pos? (list 1 1 1) 3) #false)
(check-expect (uno-en-pos? (list 1 1 1) 100) #false)

; EJERCICIO 5

; obtener-grupos : Natural -> List(List(Natural))
; Toma un tamaño de bloque de un mensaje con corrección Hamming
; y devuelve las posiciones de los bits correspondientes a cada
; bit de paridad en el bloque.
(define (obtener-grupos n)
  (local
    (
     ; obtener-grupo : Natural -> List(Natural)
     ; Recibe el índice del bit en 1 de la posición de un bit de paridad
     ; y devuelve las posiciones de los bits del bloque sobre las que predica
     (define (obtener-grupo pbp)
       (local
        (
         ; uno-en-pos-bp? : Binario -> Natural
         ; Recibe un Binario y chequea si tiene un 1 en el mismo índice
         ; en el que la posición de un bit de paridad tiene un 1
         (define (uno-en-pos-bp? b) (uno-en-pos? b pbp))
        )
        (map binario->natural (filter uno-en-pos-bp? (map natural->binario (posiciones n))))
       )
    )
   )
   (map obtener-grupo (posiciones (bloque-a-bits-redundancia n)))
  )
)

(check-expect (obtener-grupos 16)
              (list (list 1 3 5 7 9 11 13 15) ; posiciones asociadas al bit de paridad en 1
                    (list 2 3 6 7 10 11 14 15) ; posiciones asociadas al bit de paridad en 2
                    (list 4 5 6 7 12 13 14 15) ; posiciones asociadas al bit de paridad en 4
                    (list 8 9 10 11 12 13 14 15))) ; posiciones asociadas al bit de paridad en 8


; EJERCICIO 6

; list-ith : List(Any) Natural -> Any
; Recibe una lista y un índice n y devuelve
; el elemento de la lista en la posición n 
(define (list-ith l n)
  (cond [(empty? l) "índice inválido"]
        [(zero? n) (first l)]
        [(positive? n) (list-ith (rest l) (sub1 n))]
  )
)

; valores-en-posiciones : List(Any) List(Natural) -> List(Any)
; Recibe una lista de elementos de cualquier tipo
; y otra de posiciones, devuelve un lista
; con los elementos de la primera lista que están en esas posiciones
(define (valores-en-posiciones lv lp)
  (cond [(or (empty? lv) (empty? lp)) '()]
        [(not (equal? (list-ith lv (first lp)) "índice inválido"))
         (cons (list-ith lv (first lp)) (valores-en-posiciones lv (rest lp)))]
        [else (valores-en-posiciones lv (rest lp))]
  )
)

(check-expect (valores-en-posiciones (list 12 21 34 47) (list 0 1 2)) (list 12 21 34))
(check-expect (valores-en-posiciones empty (list 0 1 2)) empty)
(check-expect (valores-en-posiciones (list "a" "b" "c") empty) empty)
(check-expect (valores-en-posiciones (list 1 "d" #true) (list 0 1 3 5)) (list 1 "d"))



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