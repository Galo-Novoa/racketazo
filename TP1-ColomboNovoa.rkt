;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname TP1-ColomboNovoa) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
(require racket/bool)

; INTEGRANTES: Luca Colombo y Galo Novoa

; PREGUNTAS DE TEORÍA

; 1. ¿Y con el bit en 0 qué hacemos?

; Al bit 0 lo usamos como bit de paridad del bloque completo para saber en algunos casos si hubo error
; en el bloque o no, es el protector de los bits de paridad, protege a quien protege.


; 2. Intente convencerse de que los 4 bits de paridad cubren todo el mensaje.
; Calcule qué conjunto de bits de paridad deben fallar para exponer cada bit fallido en el mensaje.

; Para determinar cuáles bits de paridad son los que ayudan a exponer el error en cada bit b del mensaje,
; necesitamos transformar la posición b a binario, y ver en qué índices i b está en 1.
; Para exponer b, deben fallar los bits de paridad en la posición 2^i para todo i en el que b está en 1.


; 3. Convierta los naturales 20, 32 y 15 para familiarizarse con el algoritmo.

; 20 / 2 resto 0
; 10 / 2 resto 0
; 5 / 2 resto 1
; 2 / 2 resto 0
; cociente 1
; 20 es 10100 en binario

; 32 / 2 resto 0
; 16 / 2 resto 0
; 8 / 2 resto 0
; 4 / 2 resto 0
; 2 / 2 resto 0
; cociente 1
; 32 es 100000 en binario

; 15 / 2 resto 1
; 7 / 2 resto 1
; 3 / 2 resto 1
; cociente 1
; 15 es 1111 en binario


; 4. Convierta los binarios 10100, 100000 y 1111 a números naturales para familiarizarse con el algoritmo.

; 10100:
; 1 * 2^4 + 0 * 2^3 + 1 * 2^2 + 0 * 2^1 + 0 * 2^0
; = 16 + 0 + 4 + 0 + 0 = 20

; 100000:
; 1 * 2^5 + 0 * 2^4 + 0 * 2^3 + 0  * 2^2 + 0 * 2^1 + 0 * 2^0
; = 32 + 0 + 0 + 0 + 0 + 0 = 32

; 1111:
; 1 * 2^3 + 1  * 2^2 + 1 * 2^1 + 1 * 2^0
; = 8 + 4 + 2 + 1 = 15



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

; EJERCICIO 1

; paridad : List(Bit) -> Bit
; Toma un Mensaje, devuelve 1 si
; la cantidad de unos en el Mensaje es impar,
; y 0 si es par.
; Ejemplo: (paridad '()) -> 0
; Ejemplo: (paridad (list 1 1 0 0 1 0)) -> 1
; Ejemplo: (paridad (list 1 0 0 0 1 0)) -> 0
(define (paridad m)
  (cond [(empty? m) 0]
        [(= (first m) 0) (paridad (rest m))]
        [else (- 1 (paridad (rest m)))]
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
; Ejemplo: (natural->binario 0) -> '()
; Ejemplo: (natural->binario 20) -> (list 0 0 1 0 1)
; Ejemplo: (natural->binario 32) -> (list 0 0 0 0 0 1)
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
; Ejemplo: (binario->natural (list 1 1)) -> 3
; Ejemplo: (binario->natural (list 1 0 1)) -> 5
; Ejemplo: (binario->natural (list 0 1 1)) -> 6
; Ejemplo: (binario->natural (list 0 1 0 0 0 1)) -> 34
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
; Ejemplo: (uno-en-pos? (list 1 0 0) 0) -> #true
; Ejemplo: (uno-en-pos? (list 0 1 0) 1) -> #true
; Ejemplo: (uno-en-pos? (list 0 0 1) 2) -> #true
; Ejemplo: (uno-en-pos? (list 1 1 1) 3) -> #false
; Ejemplo: (uno-en-pos? (list 1 1 1) 100) -> #false
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
; Ejemplo: (obtener-grupos 16) ->
;   (list (list 1 3 5 7 9 11 13 15)
;         (list 2 3 6 7 10 11 14 15)
;         (list 4 5 6 7 12 13 14 15)
;         (list 8 9 10 11 12 13 14 15))
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
; Ejemplo: (valores-en-posiciones (list 12 21 34 47) (list 0 1 2)) -> (list 12 21 34)
; Ejemplo: (valores-en-posiciones empty (list 0 1 2)) -> empty
; Ejemplo: (valores-en-posiciones (list "a" "b" "c") empty) -> empty
; Ejemplo: (valores-en-posiciones (list 1 "d" #true) (list 0 1 3 5)) -> (list 1 "d")
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


; EJERCICIO 7

; es-potencia-de-dos? : Natural -> Bool
; Verifica si un número natural es potencia de dos
; Ejemplo: (es-potencia-de-dos? 0) -> #true
; Ejemplo: (es-potencia-de-dos? 256) -> #true
; Ejemplo: (es-potencia-de-dos? 1) -> #true
; Ejemplo: (es-potencia-de-dos? 5) -> #false
; Ejemplo: (es-potencia-de-dos? 6) -> #false
(define (es-potencia-de-dos? n)
  (cond [(zero? n) #t]
        [(zero? (sub1 n)) #t]
        [(and (even? n) (es-potencia-de-dos? (/ n 2))) #t]
        [else #f]
  )
)

(check-expect (es-potencia-de-dos? 0) #true)
(check-expect (es-potencia-de-dos? 256) #true)
(check-expect (es-potencia-de-dos? 1) #true)
(check-expect (es-potencia-de-dos? 5) #false)
(check-expect (es-potencia-de-dos? 6) #false)


; EJERCICIO 8

; ubicar-mensaje : List(Bit) -> List(Bit)
; Recibe un Mensaje y una lista de posiciones y devuelve un Bloque con sus bits
; posicionados en donde les correspondería luego de agregarle
; los bits de paridad, con 0 en las posiciones de los bits de paridad,
; la lista de posiciones que recibe es la del Bloque.
; Ejemplo: (ubicar-mensaje (list 1 0 1 0 0 1 0 1 0 0 1) (range 0 16 1)) ->
;   (list 0 0 0 1 0 0 1 0 0 0 1 0 1 0 0 1)
(define (ubicar-mensaje m lp)
       (cond [(or (empty? m) (empty? lp)) '()]
             [(es-potencia-de-dos? (first lp)) (cons 0 (ubicar-mensaje m (rest lp)))]
             [else (cons (first m) (ubicar-mensaje (rest m) (rest lp)))]
       )
)

(check-expect (ubicar-mensaje (list 1 0 1 0 0 1 0 1 0 0 1)
                              (range 0 16 1))
              (list 0 0 0 1 0 0 1 0 0 0 1 0 1 0 0 1))


; EJERCICIO 9

; calcular-paridades : List(List(Natural)) List(List(Bit)) -> List(Bit)
; Recibe lista de grupos de paridad de un mensaje y bloque sin paridades,
; devuelve lista de paridades correspondientes al bloque.
; Ejemplo: (calcular-paridades (obtener-grupos 16)
;                              (list 0 0 0 0 0 0 1 1 0 0 0 0 1 1 1 0)) ->
;   (list 0 1 1 1)
; Ejemplo: (calcular-paridades (obtener-grupos 16)
;                              (list 0 0 0 1 0 0 1 0 0 0 1 0 1 0 0 1)) ->
;   (list 0 0 1 1)
(define (calcular-paridades grupos bloque)
    (cond [(empty? grupos) '()]
          [else (cons
                 (paridad (valores-en-posiciones bloque (first grupos)))
                 (calcular-paridades (rest grupos) bloque)
                )
          ]
    )
)

(check-expect (calcular-paridades (obtener-grupos 16)
                                  (list 0 0 0 0
                                        0 0 1 1
                                        0 0 0 0
                                        1 1 1 0))
              (list 0 1 1 1))
(check-expect (calcular-paridades (obtener-grupos 16)
                                  (list 0 0 0 1
                                        0 0 1 0
                                        0 0 1 0
                                        1 0 0 1))
              (list 0 0 1 1))


; EJERCICIO 10

; emparejar : List(X) List(Y) -> List(Par[X,Y])
; Toma dos listas, devuelve lista de n pares
; compuestos por el elemento n de ambas
; solo si existe elemento en esa posicion en ambas.
; Ejemplo: (emparejar '() '()) -> '()
; Ejemplo: (emparejar '() (list 1 2 3)) -> '()
; Ejemplo: (emparejar (list 4 5 6) '()) -> '()
; Ejemplo: (emparejar (list 1 2 3) (list 4 5 6)) ->
;   (list (make-Par 1 4) (make-Par 2 5) (make-Par 3 6))
; Ejemplo: (emparejar (list 1 2 3 4 5) (list 6 7 8)) ->
;   (list (make-Par 1 6) (make-Par 2 7) (make-Par 3 8))
; Ejemplo: (emparejar (list 1 2 3) (list 4 5 6 7 8)) ->
;   (list (make-Par 1 4) (make-Par 2 5) (make-Par 3 6))
; Ejemplo: (emparejar (list 42) (list 99)) -> (list (make-Par 42 99))
; Ejemplo: (emparejar (list 1 2 3) (list 4 5)) ->
;   (list (make-Par 1 4) (make-Par 2 5))
(define (emparejar l1 l2)
  (cond [(or (empty? l1) (empty? l2)) '()]
        [else (cons (make-Par (first l1) (first l2)) (emparejar (rest l1) (rest l2)))]
  )
)

(check-expect (emparejar '() '()) '())
(check-expect (emparejar '() (list 1 2 3)) '())
(check-expect (emparejar (list 4 5 6) '()) '())

(check-expect (emparejar (list 1 2 3) (list 4 5 6))
              (list (make-Par 1 4)
                    (make-Par 2 5)
                    (make-Par 3 6)))

(check-expect (emparejar (list 1 2 3 4 5) (list 6 7 8))
              (list (make-Par 1 6)
                    (make-Par 2 7)
                    (make-Par 3 8)))

(check-expect (emparejar (list 1 2 3) (list 4 5 6 7 8))
              (list (make-Par 1 4)
                    (make-Par 2 5)
                    (make-Par 3 6)))

(check-expect (emparejar (list 42) (list 99))
              (list (make-Par 42 99)))


; EJERCICIO 11

; ubicar-paridades : List(Bit) List(Bit) -> List(Bit)
; Recibe lista de paridades incluido bit de posicion 0
; y Bloque sin paridades colocadas, devuelve Bloque
; con las paridades ubicadas.
; Ejemplo: (ubicar-paridades (list 0 1 1 1 0)
;                            (list 0 0 0 1 0 0 1 0 0 0 1 0 1 0 0 1)) ->
;   (list 0 1 1 1 1 0 1 0 0 0 1 0 1 0 0 1)
(define (ubicar-paridades lp bloque)
  (local
    (
     (define POSICIONES-BLOQUE (posiciones (length bloque)))
     ; auxiliar List(Bit) List(Bit) List(Natural) -> List(Bit)
     ; Hace lo mismo que ubicar-paridades pero recibe
     ; POSICIONES-BLOQUE como argumento para poder recorrerla
     (define (auxiliar lpx pos-bloq)
        (cond [(empty? pos-bloq) '()]
              [(es-potencia-de-dos? (first pos-bloq)) (cons (first lpx) (auxiliar (rest lpx) (rest pos-bloq)))]
              [else (cons (list-ith bloque (first pos-bloq)) (auxiliar lpx (rest pos-bloq)))]
        )
    )
   )
   (auxiliar lp POSICIONES-BLOQUE)
  )
)

(check-expect (ubicar-paridades (list 0 1 1 1 0)
                                (list 0 0 0 1
                                      0 0 1 0
                                      0 0 1 0
                                      1 0 0 1))
              (list 0 1 1 1
                    1 0 1 0
                    0 0 1 0
                    1 0 0 1))


; EJERCICIO 12

; mensaje-a-bloque : List(Bit) -> List(Bit)
; Recibe un Mensaje (lista de Bits) y lo
; devuelve codificado con bits de paridad de Hamming
; Ejemplo: (mensaje-a-bloque MENSAJE1) ->
;   (list 0 0 1 0 1 0 1 1 1 0 0 0 1 1 1 0)
; Ejemplo: (mensaje-a-bloque MENSAJE2) ->
;   (list 1 0 0 1 1 0 1 0 1 0 1 0 1 0 0 1)
; Ejemplo: (mensaje-a-bloque MENSAJE3) ->
;   (list 0 1 1 1 0 0 1 0 0 0 1 0 1 0 0 0)
; Ejemplo: (mensaje-a-bloque (list 1 0 1 0)) ->
;   (list 0 1 0 1 1 0 1 0)
; Ejemplo: (mensaje-a-bloque (list 0 0 1 1 0 0 0 1 1 1 0)) ->
;   (list 0 0 1 0 1 0 1 1 1 0 0 0 1 1 1 0)
(define (mensaje-a-bloque m)
  (local
    
    (
     (define LARGO-MENSAJE (tamaño-bloque (length m)))
     (define BLOQUE-SIN-PARIDADES (ubicar-mensaje m (posiciones LARGO-MENSAJE)))
     (define BLOQUE-SIN-BIT-0 (ubicar-paridades
                              (cons 0 (calcular-paridades (obtener-grupos LARGO-MENSAJE) BLOQUE-SIN-PARIDADES))
                              BLOQUE-SIN-PARIDADES)
     )
     (define  BIT-0 (paridad BLOQUE-SIN-BIT-0))
    )
    (cons BIT-0 (rest BLOQUE-SIN-BIT-0))
 )
)

(check-expect (mensaje-a-bloque (list 0
                                      0 1 1
                                      0 0 0
                                      1 1 1 0))
              (list 0 0 1 0
                    1 0 1 1
                    1 0 0 0
                    1 1 1 0))

(check-expect (mensaje-a-bloque (list 1 0 1 0))
              (list 0 1 0 1 1 0 1 0))

(check-expect (mensaje-a-bloque MENSAJE1)
              (list 0 0 1 0 1 0 1 1 1 0 0 0 1 1 1 0))

(check-expect (mensaje-a-bloque MENSAJE2)
              (list 1 0 0 1 1 0 1 0 1 0 1 0 1 0 0 1))

(check-expect (mensaje-a-bloque MENSAJE3)
              (list 0 1 1 1 0 0 1 0 0 0 1 0 1 0 0 0))

