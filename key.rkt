#lang racket

; provide methods for genrating RSA keys
(provide get_keys get&save_keys)

; required modules
(require math)
(require "prime.rkt")
(require "utils.rkt")
(require "file.rkt")

; minimum size for public exponent e
(define min_e 100)

; generates the public key for RSA encryption
(define (get_public_key phi)
  (let rec([n (random-integer min_e phi)]) (
              if (not (= (gcd n phi) 1))
                  (rec (++ n))
                    n
                    
                  )
              )
    )

; generates the private key for RSA encryption
(define (get_private_key e phi)
  (modular-inverse e phi)
  )


; generates RSA key pair for encryption&decryption
(define (get_keys)
  
  (define p (random_prime))
  (define q (random_prime))
  (define n (* p q))
  (define phi (* (-- p) (-- q)))

  (define e (get_public_key phi))

  (let rec ([d (get_private_key e phi)])
    (if (= e d)
        (rec)
         (list (list e n) (list d n))
        )
    )
  )


; generates RSA key pair and saves them to the current dir
(define (get&save_keys)
  (let ([keys (get_keys)])
    (save->file "pub_key.txt" (list-ref keys 0))
    (save->file "priv_key.txt" (list-ref keys 1))
    )
  )
