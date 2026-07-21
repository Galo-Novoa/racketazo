;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname practica-7) (read-case-sensitive #t) (teachpacks ((lib "testing.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "testing.rkt" "teachpack" "htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))

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


; intervalo : Natural Natural -> List(Natural)
(define (intervalo a b)
  (cond [(> a b) '()]
        [else (cons a (intervalo (add1 a) b))]
  )
)

; eliminar-multiplos : Natural List(Natural) -> List(Natural)
(define (eliminar-multiplos n l)
  (cond [(empty? l) '()]
        [(= (modulo (first l) n) 0) (eliminar-multiplos n (rest l))]
        [else (cons (first l) (eliminar-multiplos n (rest l)))]
  )
)

; eratostenes : List(Natural) -> List(Natural)
(define (eratostenes l)
  (cond [(empty? l) '()]
        [else (cons (first l) (eratostenes (eliminar-multiplos (first l) l)))]
  )
)

(check-expect (eratostenes (intervalo 2 10)) (list 2 3 5 7))
(check-expect (eratostenes empty) empty)

; criba-eratostenes : Natural -> List(Natural)
(define (criba-eratostenes n) (eratostenes (intervalo 2 n)))

(check-expect (criba-eratostenes 2) (list 2))
(check-expect (criba-eratostenes 3) (list 2 3))
(check-expect (criba-eratostenes 4) (list 2 3))
(check-expect (criba-eratostenes 5) (list 2 3 5))
(check-expect (criba-eratostenes 20) (list 2 3 5 7 11 13 17 19))
(check-expect (criba-eratostenes 30) (list 2 3 5 7 11 13 17 19 23 29))
(check-expect (criba-eratostenes 100) (list 2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 53 59 61 67 71 73 79 83 89 97))

(define (new-line s) (string-append s "\n"))
; (write-file "primos.txt" (foldr string-append "" (map new-line (map number->string (criba-eratostenes 10000)))))

(define (implica p q) (if p (if q #t #f) #t))
(define (equivalencia p q) (if (equal? p q) #t #f))
(define (valuaciones n)
  (local (
          (define (agregar-t l) (cons #t l))
          (define (agregar-f l) (cons #f l))
         )
          (cond [(zero? n) (list '())]
          [else (append (map agregar-f (valuaciones (sub1 n))) (map agregar-t (valuaciones (sub1 n))))]
          )
   )
)

(check-expect (valuaciones 3) (list
 (list #false #false #false)
 (list #false #false #true)
 (list #false #true #false)
 (list #false #true #true)
 (list #true #false #false)
 (list #true #false #true)
 (list #true #true #false)
 (list #true #true #true))
)

(define (B l)
 (let ([p1 (first l)]
        [p2 (second l)]
        [p3 (third l)])
  (equivalencia
   (implica (and p1 p2) p3)
   (and (implica p1 p3) (implica p2 p3))
  )
 )
)

(define (C l)
  (let ([p1 (first l)]
        [p2 (second l)]
       )
    (equivalencia (or (not p1) (not p2)) (and p1 p2))
  )
)

(define (evaluar p n)
  (map p (valuaciones n))
)

(define (and2 p1 p2) (and p1 p2))
(define (or2 p1 p2) (or p1 p2))

(define (tautologia? p n) (foldr and2 #t (evaluar p n)))
(define (satisfactible? p n) (foldr or2 #f (evaluar p n)))
(define (contradiccion? p n) (not (satisfactible? p n)))


; ============================================================
; TESTS PARA TAUTOLOGÍA, SATISFACTIBLE Y CONTRADICCIÓN
; ============================================================

; 1. Tautología: p ∨ ¬p (siempre verdadera)
(define (p-o-no-p v)
  (or (first v) (not (first v))))

(check-expect (tautologia? p-o-no-p 1) #t)
(check-expect (satisfactible? p-o-no-p 1) #t)
(check-expect (contradiccion? p-o-no-p 1) #f)

; 2. Contradicción: p ∧ ¬p (siempre falsa)
(define (p-y-no-p v)
  (and (first v) (not (first v))))

(check-expect (tautologia? p-y-no-p 1) #f)
(check-expect (satisfactible? p-y-no-p 1) #f)
(check-expect (contradiccion? p-y-no-p 1) #t)

; 3. p ∨ q (satisfactible, no tautología, no contradicción)
(define (p-o-q v)
  (or (first v) (second v)))

(check-expect (tautologia? p-o-q 2) #f)
(check-expect (satisfactible? p-o-q 2) #t)
(check-expect (contradiccion? p-o-q 2) #f)

; 4. p ∧ q (satisfactible, no tautología, no contradicción)
(define (p-y-q v)
  (and (first v) (second v)))

(check-expect (tautologia? p-y-q 2) #f)
(check-expect (satisfactible? p-y-q 2) #t)
(check-expect (contradiccion? p-y-q 2) #f)

; 5. p → p (tautología)
(define (implica-p-p v)
  (implica (first v) (first v)))

(check-expect (tautologia? implica-p-p 1) #t)
(check-expect (satisfactible? implica-p-p 1) #t)
(check-expect (contradiccion? implica-p-p 1) #f)

; 6. p → q (satisfactible, no tautología, no contradicción)
(define (p-implica-q v)
  (implica (first v) (second v)))

(check-expect (tautologia? p-implica-q 2) #f)
(check-expect (satisfactible? p-implica-q 2) #t)
(check-expect (contradiccion? p-implica-q 2) #f)

; 7. p ↔ p (tautología)
(define (equiv-p-p v)
  (equivalencia (first v) (first v)))

(check-expect (tautologia? equiv-p-p 1) #t)
(check-expect (satisfactible? equiv-p-p 1) #t)
(check-expect (contradiccion? equiv-p-p 1) #f)

; 8. p ↔ q (satisfactible, no tautología, no contradicción)
(define (equiv-p-q v)
  (equivalencia (first v) (second v)))

(check-expect (tautologia? equiv-p-q 2) #f)
(check-expect (satisfactible? equiv-p-q 2) #t)
(check-expect (contradiccion? equiv-p-q 2) #f)

; 9. p ∧ ¬p ∧ q (contradicción)
(define (p-y-no-p-y-q v)
  (and (first v) (not (first v)) (second v)))

(check-expect (tautologia? p-y-no-p-y-q 2) #f)
(check-expect (satisfactible? p-y-no-p-y-q 2) #f)
(check-expect (contradiccion? p-y-no-p-y-q 2) #t)

; 10. p ∨ ¬p ∨ q (tautología)
(define (p-o-no-p-o-q v)
  (or (first v) (not (first v)) (second v)))

(check-expect (tautologia? p-o-no-p-o-q 2) #t)
(check-expect (satisfactible? p-o-no-p-o-q 2) #t)
(check-expect (contradiccion? p-o-no-p-o-q 2) #f)
