;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname practica1) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define (ancho? img) (cond [(< (image-width img) (image-height img)) "Angosta"]
                           [(> (image-width img) (image-height img)) "Ancha"]
                           [#t "Cuadrada"]))

(define (equilatero? a b c) (cond [(not (= (+ a b c) 180)) "No es triangulo"]
                                  [(= a b c) "Equilatero"]
                                  [(or (= a b) (= a c) (= b c)) "isosceles"]
                                  [#t "escaleno"]))
(define PL 8)
(define PC 60)

(define (precio c l) (cond [(> (+ c l) 9) (* 0.82 (+ (* PL l) (* PC c)))]
                           [(and (> c 3) (> l 4)) (+ (* PL l 0.85) (* PC c 0.9))]
                           [(> c 3) (+ (* PL l) (* PC c 0.9))]
                           [(> l 4) (+ (* PL l 0.85) (* PC c))]
                           [#t (+ (* PL l) (* PC c))]))

(define (pitagorica? a b c) (cond [(= (* a a) (+ (* b b) (* c c))) #t]
                                  [(= (* b b) (+ (* a a) (* c c))) #t]
                                  [(= (* c c) (+ (* b b) (* a a))) #t]
                                  [else #f]))

(define (anchofinoli? img) (cond [(< (* 2 (image-width img)) (image-height img)) "Muy angosta"]
                                 [(< (image-width img) (image-height img)) "Angosta"]
                                 [(> (image-width img) (* 2 (image-height img))) "Muy ancha"]
                                 [(> (image-width img) (image-height img)) "Ancha"]
                                 [#t "Cuadrada"]))

(define (clasificar t) (cond [(< t 0) "Muy frío (MF)"]
                        [(and (>= t 0) (< t 15)) "Frío (F)"]
                        [(and (>= t 15) (< t 25)) "Agradable (A)"]
                        [(>= t 25) "Caluroso (C)"]))

(define (sgn2 x) (cond [(< x 0) -1]
                       [(= x 0) 0]
                       [(> x 0) 1]))

(define (imgaux x) (cond [(string=? "Ancha" x) 1]
                         [(string=? "Angosta" x) -1]
                         [else 0]))

(define (sgn3 x) (cond [(number? x) (sgn2 x)]
                       [(and (string? x) (number? (string->number x))) (sgn2 (string->number x))]
                       [(boolean? x) (sgn2 (if x 1 0))]
                       [(image? x) (sgn2 (imgaux (ancho? x)))]
                       [else "Clase no soportada por la función."]))