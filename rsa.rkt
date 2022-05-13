#lang racket

(provide encrypt decrypt)

(require "utils.rkt")
(require "file.rkt")

(require math)

; encrypts
(define (encrypt s pub_key)
  (modular-expt (string->int s) (car pub_key) (cdr pub_key))
  )

; decrypts
(define (decrypt s priv_key)
  (int->string (modular-expt s (car priv_key) (cdr priv_key)))
  )






  
