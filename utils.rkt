#lang racket

; export function(s)
(provide gcd egcd ++ -- modulo2 string->int int->string)

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

; convert string to number
(define (string->int s)

   (for/fold ((rv 0)) ((b (string->bytes/utf-8 s))) (+ b (* rv 256)))
   ;(for/fold ((r 1)) ((i '(1 2 3 4 5))) (* r i))
  
  )

(define (int->string N)
  (define (inr n b)
    (if (zero? n)
        b
        (inr (quotient n 256) (bytes-append (bytes (modulo n 256)) b))
        )
    )
  (bytes->string/utf-8 (inr N (bytes)))
  )

;(define (test s)
;  (let rec([B (string->bytes/utf-8 s)] [value 0] [i 0])
;    (if (= i (-- (string-length s)))
;           value 
;    (rec B (+ (bytes-ref B i) (* value 256)) (++ i)))
;    
;    )
;  )



