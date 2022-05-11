#lang racket

; export function(s)
(provide gcd egcd ++ -- modulo2)

; get greatest common divisor using Euclid's algorithm
(define (gcd a b)
  (if (zero? a)
      b
      (gcd (modulo b a) a)
      )
  )

; extended euclidean algorithm
(define (egcd a b x y u v)

  (if (zero? a)
      x
      (egcd (modulo b a) a u v (- x (* u (floor (/ b a)))) (- y (* v (floor (/ b a)))))
      )
  )

; applies modulo to the passed integer by two
(define (modulo2 integer)
  (modulo integer 2)
  )

; increments the passed integer by one
(define (++ integer)
  (+ integer 1)
  )

; decrements the passed integer by one
(define (-- integer)
  (- integer 1)
  )



