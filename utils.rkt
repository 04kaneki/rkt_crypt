#lang racket

; export function(s)
(provide gcd ++ -- modulo2)

; get greatest common divisor using Euclid's algorithm
(define (gcd a b)
  (if (zero? a)
      b
      (gcd (modulo b a) a)
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



