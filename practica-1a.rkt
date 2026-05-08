;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname practica1) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "abstraction.rkt" "teachpack" "2htdp")) #f)))
(define (distancia x y) (sqrt (+(* x x)(* y y))))
(define (pitagorica? a b c) (if (or (= (* c c)(+ (* a a)(* b b))) (= (* b b)(+ (* a a)(* c c))) (= (* a a)(+ (* c c)(* b b))))
                                (string-append "Los números " (number->string a) ", " (number->string b) " y " (number->string c) " forman una terna pitagorica")
                                                            (string-append "Los números " (number->string a) ", " (number->string b) " y " (number->string c) " no forman una terna pitagorica")))
(define (triángulo? a b c) (and (< a (+ b c)) (< b (+ a c)) (< c (+ a b))))
(define (cuadernos x) (if (< 3 x) (* (* 60 x) 0.9) (* 60 x)))
(define (anchoOlargo img)
  (if (= (image-width img) (image-height img)) "cuadrada"
         (if (< (image-width img) (image-height img)) "angosta" "ancha")))

(define (angulitos a b c) (if (= (+ a b c) 180) (if (= a b c) "equilatero"
                              (if (or (= a b) (= a c) (= b c)) "isosceles" "escaleno")) "no es un triangulo"))

(define PC 60)
(define PL 8)
(define of10 0.82)

(define (precios c l) (if (> (+ c l) 10) (* of10 (+ (* PL l) (* PC c)))(if (< 3 c) (if (< 4 l) (+ (* PC c 0.9) (* PL l 0.85)) (+ (* PC c 0.9) (* PL l)))
                          (if (< 4 l) (+ (* PC c) (* PL l 0.85)) (+ (* PC c) (* PL l))))))

(define (collatz n) (if (= (remainder n 2) 0) (/ n 2) (+ (* 3 n) 1)))



(define ejemplo (place-image (rectangle 90 30 "solid" "red")
45 0
(place-image (circle 10 "solid" "blue")
45 45
(empty-scene 90 60))))


; diversion con banderas
(define H 60)
(define V 90)

(define peru (place-image (rectangle 30 60 "solid" "red")
                          15 30
                          (place-image (rectangle 30 60 "solid" "white")
                          45 30
                          (place-image (rectangle 30 60 "solid" "red")
                          75 30
                          (empty-scene 90 60)))))

(define italia (place-image (rectangle 30 60 "solid" "green")
                          15 30
                          (place-image (rectangle 30 60 "solid" "white")
                          45 30
                          (place-image (rectangle 30 60 "solid" "red")
                          75 30
                          (empty-scene 90 60)))))

(define alemania (place-image (rectangle 90 20 "solid" "black")
                          45 10
                          (place-image (rectangle 90 20 "solid" "red")
                          45 30
                          (place-image (rectangle 90 20 "solid" "yellow")
                          45 50
                          (empty-scene 90 60)))))

(define (vertical a b c) (place-image (rectangle 30 60 "solid" a)
                          15 30
                          (place-image (rectangle 30 60 "solid" b)
                          45 30
                          (place-image (rectangle 30 60 "solid" c)
                          75 30
                          (empty-scene 90 60)))))
(define (horizontal a b c) (place-image (rectangle 90 20 "solid" a)
                          45 10
                          (place-image (rectangle 90 20 "solid" b)
                          45 30
                          (place-image (rectangle 90 20 "solid" c)
                          45 50
                          (empty-scene 90 60)))))

(define (vertical/horizontal o a b c) (if (string=? o "horizontal") (horizontal a b c) (vertical a b c)))

(define sudan (place-image (rotate 270 (triangle 60 "solid" "green"))
                           25 30
                           (vertical/horizontal "horizontal" "red" "white" "black")))

(define argentina (place-image (circle 9 "solid" "yellow")
                               45 30
                               (vertical/horizontal "horizontal" "cyan" "white" "cyan")))

(define camerun (place-image (star 10 "solid" "yellow")
                             45 30
                             (vertical/horizontal "vertical" "green" "red" "yellow")))

(define brasil (place-image (circle 10 "solid" "blue")
                            (/ V 2) (/ H 2)
               (place-image (rotate -30 (triangle 40 "solid" "yellow"))
                            (- (/ V 2) 17.3) (/ H 2)
               (place-image (rotate 30 (triangle 40 "solid" "yellow"))
                            (+ (/ V 2) 17.3) (/ H 2)
               (place-image (rectangle V H "solid" "green")
                            (/ V 2) (/ H 2) 
                            (empty-scene V H))))))