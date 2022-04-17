#lang racket

(require "key.rkt")
(require "utils.rkt")
(require "file.rkt")
(require math)


; encrypts the msg with the public key
(define (encrypt msg pub_key)
  (let ([msg_list (string->list msg)])
    (let rec ([i (-- (length msg_list))] [list_crypt '()] [pub_key (if (list? pub_key) pub_key (read_file pub_key))])
      (cond
        [(zero? i) list_crypt]
        [else (rec (-- i) (append list_crypt
                                  (list (modular-expt (char->integer (list-ref msg_list i))
                                                      (list-ref pub_key 0)
                                                      (list-ref pub_key 1)
                                                      )
                                        )) pub_key)]
  
        )
      )
    
    )
  )


; decrypts the msg with the private key
(define (decrypt enc_msg prv_key)
  (let rec ([i (-- (length enc_msg))] [list_decrypt '()])
    (cond
      [(zero? i) list_decrypt]
      [else (rec (-- i) (append list_decrypt
                                (list (integer->char
                                       (modular-expt (list-ref enc_msg i)
                                                     (list-ref prv_key 0)
                                                     (list-ref prv_key 1)
                                                     )
                                       )
                                      )))]
      )
    )
  )
  