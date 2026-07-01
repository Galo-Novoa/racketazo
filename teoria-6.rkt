;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname teoria-6) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))

; Representamos los Naturales con Number no negativos
; factorial : Natural -> Natural
(define (factorial n)
  (cond [(zero? n) 1]
        [(positive? n) (* n (factorial (sub1 n)))]
  )
)


; fib : Natural -> Natural
(define (fib n)
  (cond [(zero? n) 1]
        [(zero? (sub1 n)) 1]
        [(positive? n) (+ (fib (sub1 n)) (fib (sub1 (sub1 n))))]
  )
)


; sumatoria : Natural (Natural -> Number) -> Number
(define (sumatoria i f)
  (cond [(zero? i) (f 0)]
        [(positive? i) (+ (f i) (sumatoria (sub1 i) f))]
  )
)

(check-expect (sumatoria 5 sqr) (+ 1 4 9 16 25))
(check-expect (sumatoria 0 sqr) 0)
(check-expect (sumatoria 8 identity) (+ 1 2 3 4 5 6 7 8))

; alternada : Natural -> Number
(define (alternada n)
  (cond [(zero? n) 0]
        [(even? n)
         (+ (* -1 (/ 1 n)) (alternada (sub1 n)))]
        [(odd? n)
          (+ (/ 1 n) (alternada (sub1 n)))]
  )
)

(check-expect (alternada 0) 0)
(check-expect (alternada 5) (+ 1 (/ -1 2) (/ 1 3) (/ -1 4) (/ 1 5)))


; from : Natural -> List(Natural)
(define (from n)
  (cond [(zero? n) '(0)]
        [(positive? n) (appendear (from (sub1 n)) (list n))]
  )
)

; appendear : List(X) List(X) -> List(X)
(define (appendear l1 l2)
  (cond [(empty? l1) l2]
        [(cons? l1) (cons (first l1) (appendear (rest l1) l2))]
  )
)

(check-expect (from 5) (list 0 1 2 3 4 5))
(check-expect (from 0) (list 0))


; multiplos : Natural Natural -> List(Natural)
(define (multiplos n m)
  (cond [(or (zero? n) (zero? m)) '()]
        [(positive? n) (cons (* m n) (multiplos (sub1 n) m))]
  )
)

(check-expect (multiplos 5 2) (list 10 8 6 4 2))
(check-expect (multiplos 4 7) (list 28 21 14 7))
(check-expect (multiplos 0 11) empty)


; list-ith : List(X) Natural -> X
(define (list-ith l n)
  (cond [(empty? l) "n mayor o igual a largo de lista"]
        [(zero? n) (first l)]
        [(positive? n) (list-ith (rest l) (sub1 n))]
  )
)

(check-expect (list-ith (list "a" "b" "c" "d") 2) "c")
(check-expect (list-ith (list "a" "b" "c" "d") 0) "a")
(check-expect (list-ith (list "a" "b" "c" "d") 6) "n mayor o igual a largo de lista")

