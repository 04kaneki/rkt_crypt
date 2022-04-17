#lang racket

; export function(s)
(provide save->file read_file)

; saves the content to the file, which is created if missing
(define (save->file path content)
  (let ([out (open-output-file path)])
    (write (list-ref content 0) out)
    (write " " out)
    (write (list-ref content 1) out)
    (close-output-port out)
    )
  )

; reads the file and returns the content as a string
(define (read_file path)
  (let ([in (open-input-file path)])
    (port->string in)
    )
  )