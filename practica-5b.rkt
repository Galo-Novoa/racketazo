;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname practica-5b) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; TEORIA

; filter : (X -> Bool) (Listof X) -> (Listof X)
(define (filter0 p l)
  (cond [(empty? l) '()]
        [(p (first l)) (cons (first l) (filter0 p (rest l)))]
        [else (filter0 p (rest l))]
  )
)

; map : (X -> Y) (Listof X) -> (Listof Y)
(define (map0 f l)
  (cond [(empty? l) '()]
        [else (cons (f (first l)) (map0 f (rest l)))]
  )
)

; foldr : (X Y -> Y) Y (Listof X) -> Y
(define (foldr0 f c l)
  (cond [(empty? l) c]
        [else (f (first l) (foldr0 f c (rest l)))]
  )
)

(define (menor100? n) (< n 100))

; suma-cuadrados : Listof Number -> Number
(define (suma-cuadrados l) (foldr0 + 0 (map0 sqr (filter0 menor100? l))))


; PRACTICA

; eliminar : Number (Listof Number) -> Listof Number
(define (eliminar m l)
   (local
     ((define (distinto-m? n) (not (= n m))))
     (filter distinto-m? l)
   )
)

; mayores : (Listof Number) Number -> (Listof Number)
(define (mayores l m)
  (local
    ((define (mayor-m? n) (> n m)))
    (filter mayor-m? l)
  )
)

; largas : (Listof String) Number -> Listof String
(define (largas l m)
  (local
    ((define (larga-m? n) (> (string-length n) (string-length m))))
    (filter larga-m? l)
  )
)

; sumar : (Listof Number) Number -> (Listof Number
(define (sumar l m)
  (local
    ((define (sumar-m n) (+ n m)))
    (map sumar-m l)
  )
)

; elevar : (Listof Number) Number -> (Listof Number)
(define (elevar l m)
  (local
    ((define (elevar-m n) (expt n m)))
    (map elevar-m l)
  )
)

; saa : (Listof Images) Number -> Number
(define (saa l m)
  (local
    (
     (define (ancha-m? i) (> (image-width i) m))
     (define (area-img i) (* (image-width i) (image-height i)))
    )
    (foldr + 0 (map area-img (filter ancha-m? l)))
  )
)

; algun-pos : (Listof (Listof Number)) -> Bool
(define (algun-pos l)
  (local
    ((define (sum-pos? xs) (positive? (foldr + 0 xs))))
    (cond [(empty? (filter sum-pos? l)) #f]
          [else #t]
    )
  )
)

; Ejercicio 32

(define-struct alumno [nombre nota faltas])

; destacados : (Listof Alumno) -> (Listof Alumno)
(define (destacados l)
  (local
    ((define (nota-mayor-9? a) (>= ((alumno-nota a) 9))))
    (map alumno-nombre (filter nota-mayor-9? l))
  )
)

; condicion : Alumno -> String
(define (condicion a)
  (cond [(>= (alumno-nota a) 8) "Promovido"]
        [(>= (alumno-nota a) 6) "Regular"]
        [else "Libre"]
  )
)

; exito ; (Listof Alumno) -> Bool
(define (exito l)
  (local
    ((define (esta-libre? a) (string=? (condicion a) "Libre")))
    (cond [(empty? (filter esta-libre? (map condicion l))) #t]
          [else #f]
    )
  )
)

; falta-regulares : (Listof Alumno) -> Number
(define (falta-regulares l)
  (local
    ((define (esta-regular? a) (string=? (condicion a) "Regular")))
    (foldr + 0 (map alumno-faltas (filter esta-regular? l)))
   )
)

; promovidos-ausentes : (Listof Alumno) -> (Listof Alumno)
(define (promovidos-ausentes l)
  (local
    (
     (define (esta-promovido? a) (string=? (condicion a) "Promovido"))
     (define (faltas-mayor-2? a) (>= (alumno-faltas a) 3))
    )
    (map alumno-nombre (filter esta-promovido? (filter faltas-mayor-2? l)))
  )
)