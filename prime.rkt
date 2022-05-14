#lang racket

; export function(s)
(provide random_prime)

(require math)
(require "utils.rkt")

; bit size of the prime
(define prime_size 1024)

; the number of rabin miller trials
(define num_trials 128)

; returns a random int with the given size
; max 2**n-1
; min 2**(n-1)+1
(define (nBitRandom n)
  (random-integer
   (++ (expt 2 (-- n)))
   (-- (expt 2 n))
   )
  )

; pre generated primes
(define prime_list (list
                    2 3 5 7 11 13 17 19 23 29
                    31 37 41 43 47 53 59 61 67
                    71 73 79 83 89 97 101 103
                    107 109 113 127 131 137 139
                    149 151 157 163 167 173 179
                    181 191 193 197 199 211 223
                    227 229 233 239 241 251 257
                    263 269 271 277 281 283 293
                    307 311 313 317 331 337 347 349)

  )

; low level test to check for prime
(define (test_low_lvl n)
  (let rec ([i 0])
    (cond
      [(= (length prime_list) i) true]
      [(and
        (if (zero? (modulo n (list-ref prime_list i))) true false )
        (if (<= (sqr (list-ref prime_list i)) n) true false))
       false ]
      [else (rec (++ i))]
      )
    )
  )

; get all values for deviding p-1 by 2
(define (get_values n)
  (let rec ([max_div 0] [ec (-- n)])
    (if (zero? (modulo2 ec))
        (rec (++ max_div) (/ ec 2))
        (list max_div ec)
        )
    )
  )

; the miller rabin trial 
(define (mr_trial test_num n ec max_div)
  (let rec([i 0])
    (cond
      [(= max_div i) true]
      [(and
        (= (modular-expt test_num ec n) 1)
        (zero? i))
       false]
      [(=
        (modular-expt test_num (* (expt 2 i) ec) n)
        (-- n))
       false]
      [else (rec (++ i))]
      )
    )
  )

(define (test n [num_trials num_trials])
  (let* ([numbers (let rec ([max_div 0] [ec (-- n)])
                    (if (zero? (modulo2 ec))
                        (rec (++ max_div) (/ ec 2))
                        (list max_div ec)
                        )
                    )
                  ]
         [max_div (list-ref numbers 0)][ec (list-ref numbers 1)])
    (display "A")
    )
  )



; miller rabin test
(define (miller_rabin_test n [ec 0] [max_div 0] [num_trials num_trials])
  (cond
    [(zero? num_trials) true]
    [(and (zero? ec) (zero? max_div))
     (let trial
       ([numbers (get_values n)])
       (if
        (mr_trial (random-integer 2 n) n (list-ref numbers 1) (list-ref numbers 0))
        false
        (miller_rabin_test n (list-ref numbers 1) (list-ref numbers 0) (- num_trials 1))
        )
       )]

    [else (if
           (mr_trial (random-integer 2 n) n ec max_div)
           false
           (miller_rabin_test n ec max_div (- num_trials 1))
           )]

                              
    )
  )


; get random n prime
(define (random_prime [prime_size prime_size])
  (let test_prime ([prime_candidate (nBitRandom prime_size)])
    (if (test_low_lvl prime_candidate)
        (if (miller_rabin_test prime_candidate)
            prime_candidate
            (random_prime prime_size))
        (random_prime prime_size))
    )
  )
