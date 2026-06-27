;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname practica-6) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; copiar : Natural String -> List(String)
(define (copiar n s)
  (cond [(zero? n) '()]
        [(positive? n) (cons s (copiar (sub1 n) s))]
  )
)

(check-expect (copiar 2 "hola") (list "hola" "hola"))
(check-expect (copiar 0 "hola") '())
(check-expect (copiar 4 "abc") (list "abc" "abc" "abc" "abc"))


; sumanat : Natural Natural -> Natural
(define (sumanat n m)
  (cond [(zero? n) m]
        [else (sumanat (sub1 n) (add1 m))]
  )
)

; Sin recursión de cola: [else (add1 (sumanat (sub1 n) m))]

(check-expect (sumanat 5 9) 14)
(check-expect (sumanat 5 0) 5)


; multnat : Natural Natural -> Natural
(define (multnat n m)
  (local
    ((define (aux m acum)
                  (cond [(zero? m) acum]
                        [else (aux (sub1 m) (sumanat acum n))]
                  )
    ))
    (cond [(zero? m) 0]
           [else (aux m 0)]
    )
  )
)

; Sin recursión de cola: [else (sumanat n (multnat n (sub1 m)))]


; powernat : Natural Natural -> Natural
(define (powernat n m)
  (cond [(zero? m) 1]
        [else (multnat n (powernat n (sub1 m)))]
  )
)

; factnat : Natural -> Natural
(define (factnat n)
  (cond [(zero? n) 1]
        [(positive? n) (multnat n (factnat (sub1 n)))]
  )
)

; fibnat : Natural -> Natural
(define (fibnat n)
  (cond [(zero? n) 1]
        [(zero? (sub1 n)) 1]
        [else (sumanat (fibnat (sub1 n)) (fibnat (sub1 (sub1 n))))]
  )
)

; sigma : Natural (Natural -> Number) -> Number
(define (sigma n f)
  (cond [(zero? n) (f n)]
        [(positive? n) (+ (f n) (sigma (sub1 n) f))]
  )
)

(check-expect (sigma 0 sqr) 0)
(check-expect (sigma 4 sqr) 30)
(check-expect (sigma 10 identity) 55)


; EJERCICIO 7

; R : Natural -> Number
(define (R n)
  (local
    ((define (aux i) (/ 1 (powernat 2 i))))
    (sigma n aux)
  )
)

; S : Natural -> Number
(define (S n)
  (local
    ((define (aux i) (/ i (add1 i))))
    (sigma n aux)
  )
)

; T : Natural -> Number
(define (T n)
  (local
    ((define (aux i) (/ 1 (add1 i))))
    (sigma n aux)
  )
)

; R
(check-within (R 0) 1 0.0001)
(check-within (R 1) 1.5 0.0001)
(check-within (R 2) 1.75 0.0001)
(check-within (R 3) 1.875 0.0001)
(check-within (R 4) 1.9375 0.0001)
(check-within (R 10) 1.9990234375 0.0001)

; S
(check-within (S 0) 0 0.0001)
(check-within (S 1) 0.5 0.0001)
(check-within (S 2) 1.1666666666666667 0.0001)
(check-within (S 3) 1.9166666666666667 0.0001)
(check-within (S 4) 2.716666666666667 0.0001)

; T
(check-within (T 0) 1 0.0001)
(check-within (T 1) 1.5 0.0001)
(check-within (T 2) 1.8333333333333333 0.0001)
(check-within (T 3) 2.083333333333333 0.0001)
(check-within (T 4) 2.283333333333333 0.0001)


; componer -> (Number -> Number) Natural Number -> Number
(define (componer f n x)
  (cond [(zero? n) x]
        [(positive? n) (f (componer f (sub1 n) x))]
  )
)

(check-expect (componer sqr 2 5) 625)
(check-expect (componer add1 5 13) 18)


; intervalo : Natural -> List(Natural)
(define (intervalo n)
  (cond [(zero? n) (list 0)]
        [(positive? n) (cons n (intervalo (sub1 n)))]
  )
)

; multiplos -> Natural Natural -> List(Natural)
(define (multiplos n m)
  (cond [(zero? n) '()]
        [(positive? n) (cons (multnat m n) (multiplos (sub1 n) m))]
  )
)

(check-expect (multiplos 4 7) (list 28 21 14 7))
(check-expect (multiplos 0 11) empty)


; list-fibonacci-map : Natural -> List(Natural)
(define (list-fibonacci-map n)
  (map fibnat (intervalo n))
)

; list-fibonacci-rec : Natural -> List(Natural)
(define (list-fibonacci-rec n)
  (cond [(zero? n) (list 1)]
        [(positive? n) (cons (fibnat n) (list-fibonacci-rec (sub1 n)))]
  )
)

(check-expect (list-fibonacci-rec 4)
              (list 5 3 2 1 1))
(check-expect (list-fibonacci-rec 0)
              (list 1))

(check-expect (list-fibonacci-map 4)
              (list 5 3 2 1 1))
(check-expect (list-fibonacci-map 0)
              (list 1))


; cuotas : Number Natural Natural -> List(Number)
(define (cuotas m c i)
   (local
    ((define (calc j) (+ (/ m c) (* (/ m c) (/ i (* 100 12)) j))
    ))
    (map calc (rest (reverse (intervalo c))))
  )
)

(check-expect (cuotas 10000 0 18)
              empty)
(check-expect (cuotas 10000 1 12)
              (list 10100))
(check-expect (cuotas 30000 3 12)
              (list 10100 10200 10300))
(check-expect (cuotas 100000 4 18)
              (list 25375 25750 26125 26500))


; circulos : Natural -> Image
(define (circulos m)
  (local
    (
     (define LADO (* 2 (sqr m)))
     (define FONDO (empty-scene LADO LADO "white"))
     (define (cir n)
       (cond [(zero? n) FONDO]
             [(positive? n) (place-image (circle (sqr n) "outline" "blue") (/ LADO 2) (/ LADO 2) (cir (sub1 n)))]
       )
     )
    ) 
    (cir m)
  )
)

; cuadrados : Natural Natural -> Image
(define (cuadrados n ang)
  (local
    (
     (define LADO 200)
     (define FONDO (empty-scene LADO LADO "white"))
     (define ROTACION 20)
     (define (cuad m a)
       (cond [(zero? m) FONDO]
             [(positive? m) (place-image (rotate a (square (sqr m) "outline" "blue")) (/ LADO 2) (/ LADO 2) (cuad (sub1 m) (+ a ROTACION)))]
       )
     )
    )
    (cuad n ang)
 )
)


; list-insert -> List(X) X Natural -> List(X)
(define (list-insert l x i)
  (cond [(empty? l) (cons x '())]
        [(zero? i) (cons x l)]
        [else (cons (first l) (list-insert (rest l) x (sub1 i)))]
  )
)

(check-expect (list-insert (list 5 9 10 -2) 8 0) (list 8 5 9 10 -2))
(check-expect (list-insert (list 5 9 10 -2) 8 2) (list 5 9 8 10 -2))
(check-expect (list-insert (list 5 9 10 -2) 8 4) (list 5 9 10 -2 8))
(check-expect (list-insert empty 8 3) (list 8))


; tomar : List(X) Natural -> List(X)
(define (tomar l n)
  (cond [(or (empty? l) (zero? n)) '()]
        [else (cons (first l) (tomar (rest l) (sub1 n)))]
  )
)

(check-expect (tomar (list 5 2 1 9) 0) empty)
(check-expect (tomar (list 5 2 1 9) 2) (list 5 2))
(check-expect (tomar (list 5 2 1 9) 4) (list 5 2 1 9))
(check-expect (tomar (list 5 2 1 9) 7) (list 5 2 1 9))


; tirar : List(Natural) Natural -> List(Natural)
(define (tirar l n)
  (cond [(empty? l) '()]
        [(zero? n) l]
        [else (tirar (rest l) (sub1 n))]
  )
)

(check-expect (tirar (list 5 2 1) 4) empty)
(check-expect (tirar (list 5 2 1 9) 2) (list 1 9))


; eliminar-n : List(X) X Natural -> List(X)
(define (eliminar-n l x n)
  (cond [(empty? l) '()]
        [(zero? n) l]
        [(equal? x (first l)) (eliminar-n (rest l) x (sub1 n))]
        [else (cons (first l) (eliminar-n (rest l) x n))]
  )
)

(check-expect (eliminar-n (list 1 2 1 1 1 2 1) 1 3) (list 2 1 2 1))
(check-expect (eliminar-n (list 1 2 2 1 2) 1 3) (list 2 2 2))


; member-n List(X) X Natural -> Bool
(define (member-n l x n)
  (cond [(empty? l) (if (zero? n) #t #f)]
        [(zero? n) (if (= x (first l)) #f (member-n (rest l) x n))]
        [(equal? x (first l)) (member-n (rest l) x (sub1 n))]
        [else (member-n (rest l) x n)]
  )
)

(check-expect (member-n (list 1 2 1 1 1 2 1) 1 5) #t)
(check-expect (member-n (list 1 2 2 1 2) 1 3) #f)
(check-expect (member-n (list 2 1 2 1 2 2) 2 3) #f)
