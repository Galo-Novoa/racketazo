;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname diseñodeprogramas) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; Representamos las coordenadas x e y como number y el resultado tambien
; distancia-origen : Number Number -> Number
; modificar el enunciado xd

; entrada:  3  4 salida: 5
; entrada: -3 -4 salida: 5
; entrada: -3  4 salida: 5
; entrada:  3 -4 salida: 5
; entrada:  0  5 salida: 5
; entrada:  5  0 salida: 5
; entrada:  0  0 salida: 5

(define (distancia-origen x y)
  (sqrt (+ (* x x) (* y y)))
  )

; Number Number Number Number -> Number
(define (distancia-puntos x1 y1 x2 y2)
                                        (sqrt (+ (* (- x2 x1) (- x2 x1)) (* (- y2 y1) (- y2 y1))))
)

(define (vol-cubo x)
  (* x x x)
)

(define (area-cubo x) (
                       * 6 x x
                       ))

(define (string-insert palabra i) (
               string-append (substring palabra 0 (- i 1)) "-" (substring palabra i (string-length palabra))
               ))

(define (string-last string) (string-ith string (- (string-length string) 1)))

(define (string-remove-last string) (substring string 0 (- (string-length string) 1)))

(check-expect (string-remove-last "patos") "pato")

(define precio-base 650)
(define (descuento-p p) (cond [(> p 2) 0.2]
  [(> p 1) 0.1]
  [else 0]))
(define (descuento-m m) (cond [(> m 2) 0.25]
  [(> m 1) 0.15]
  [else 0]))
(define (monto-persona p m) (* precio-base (- 1 (cond [(> (+ (descuento-p p) (descuento-m m)) 0.35) 0.35]
                                  [else (+ (descuento-p p) (descuento-m m))])))
)

;; --- Pruebas para la función monto-persona ---

;; Caso 1: Una sola persona, un solo mes (Sin descuentos)
;; 650 * (1 - 0) = 650
(check-expect (monto-persona 1 1) 650)

;; Caso 2: 2 amigos, 1 mes (Solo descuento por personas)
;; 10% de descuento -> 650 * 0.9 = 585
(check-expect (monto-persona 2 1) 585)

;; Caso 3: 1 persona, 2 meses (Solo descuento por meses)
;; 15% de descuento -> 650 * 0.85 = 552.5
(check-expect (monto-persona 1 2) 552.5)

;; Caso 4: 2 amigos, 2 meses (Combinación sin pasar el tope)
;; 10% + 15% = 25% -> 650 * 0.75 = 487.5
(check-expect (monto-persona 2 2) 487.5)

;; Caso 5: 3 amigos, 3 meses (Supera el tope del 35%)
;; 20% + 25% = 45% -> Se aplica tope 35% -> 650 * 0.65 = 422.5
(check-expect (monto-persona 3 3) 422.5)

;; Caso 6: 10 amigos, 10 meses (Mucho más del tope)
;; Sigue aplicando el 35% -> 422.5
(check-expect (monto-persona 10 10) 422.5)

