;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname practica-7) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
(define RAND-MAX 4294967087) ; número máximo para valores aleatorios
 
; aleatorio : Number Number -> Number
; dados dos números a y b, devuelve un número aleatorio el intervalo real [a,b].
(define (aleatorio a b)
      (+ a (* (- b a) (/  (+ 1 (random RAND-MAX)) (* 1.0 RAND-MAX)))))

; generar-puntos : Natural Number Number -> List(Posn)
(define (generar-puntos n a b)
  (cond [(zero? n) '()]
        [else (cons (make-posn (aleatorio a b) (aleatorio a b)) (generar-puntos (sub1 n) a b))]
  )
)

(define RADIO 1.0) ; radio del círculo
 
(define CENTRO (make-posn 0 0)) ; coordenadas del centro del círculo
 
(define MAX 300000) ; cantidad de puntos a generar para nuestra estimación


; modulo-posn : Posn -> Number
(define (modulo-posn p) (sqrt (+ (sqr (posn-x p)) (sqr (posn-y p)))))

; sumar-posn : Posn Posn -> Posn
(define (sumar-posn p1 p2) (make-posn (+ (posn-x p1) (posn-x p2)) (+ (posn-y p1) (posn-y p2))))

; adentro? : Posn -> Bool
(define (adentro? p)
  (if (<= (modulo-posn (sumar-posn CENTRO p)) RADIO) #t #f)
)

; lista con MAX puntos aleatorios
(define LISTA (generar-puntos MAX -1 1))
 
; lista con puntos de LISTA que están dentro del círculo 
(define ADENTRO (filter adentro? LISTA))
 
; aproxicimación del área del círculo a partir de la proporción de puntos que
; caen dentro del círculo:
(define AREA (* (/ (length ADENTRO) MAX) 4))