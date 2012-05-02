(module Message racket
  (require (file "Common.rkt"))
  
  (define (make-message data [role 0] [encoded #t])
    (cons encoded (cons role data)))
  
  (define (message-encoded? mess)
    (car mess))
  
  (define (get-message-role mess)
    (car (cdr mess)))
  
  (define (get-message-data mess)
    (cdr (cdr mess)))
  
  (proivde make-message message-encoded? get-message-role get-message-data))