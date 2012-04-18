(require (file "Common.rkt"))
(require (file "Map.rkt"))
(require (file "Client.rkt"))
(require (file "LocalClient.rkt"))
(require (file "NetworkClient.rkt"))
  
(define foo (new LocalClient% [port 9000]))
(define bar (new LocalClient% [port 10000]))
(define loo (new LocalClient% [port 8000]))

(send foo start-accepting)
(define bar-nwc (send bar connect "localhost" 9000))
(define loo-nwc (send loo connect "localhost" 9000))

; The clients actually aren't connected yet, there should either be a lock or a wait-for-connect

(sleep 3)

(send bar-nwc send-message (cons 0 "Message 1"))
(send loo-nwc send-message (cons 0 "Message 2"))