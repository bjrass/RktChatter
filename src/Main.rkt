(require (file "Common.rkt"))
(require (file "Map.rkt"))
(require (file "Client.rkt"))
(require (file "LocalClient.rkt"))
(require (file "NetworkClient.rkt"))

(define (funct)
  
  (define foo (new LocalClient% [port 9000]))
  (define bar (new LocalClient% [port 10000]))
  
  (send bar start-accepting)
  (define foo-nwc (send foo connect "localhost" 10000))
  
  ; The clients actually aren't connected yet, there should either be a lock or a wait-for-connect
  
  (sleep 1)
  
  (send foo-nwc send-message "Ser du det har, fungerar det, spread the word")
  
  (send foo-nwc disconnect)
  (send bar stop-accepting)
  
  (sleep 3))

(define (fuctit)
  (funct)
  (sleep 2)
  (fuctit))