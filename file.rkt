#lang racket

; export function(s)
(provide save->file read_file)

; saves the content to the file, which is created if missing
(define (save->file path key-pair)
  (let ([out (open-output-file path)])
    (writeln (car key-pair) out)
    (writeln (cdr key-pair) out)
    (close-output-port out)
    )
  )

; reads the key from the passed file
(define (read_file path)
  (let ([out (open-input-file path)])
    (cons (string->number (read-line out)) (string->number (read-line out)))
    )
  )

  



