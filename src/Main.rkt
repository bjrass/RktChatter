(require (file "Common.rkt"))
(require (file "Map.rkt"))
(require (file "Client.rkt"))
(require (file "LocalClient.rkt"))
(require (file "NetworkClient.rkt"))
  
(define bar (new LocalClient% [port 10000]))
(define foo (new LocalClient% [port 9000]))

(send foo start-accepting)

(define bar-nwc (send bar connect "localhost" 9000))

; The clients actually aren't connected yet, there should either be a lock or a wait-for-connect

(sleep 1)

(send foo create-encryption!)

(sleep 1)

(send bar set-username! "I AM FOO!")
(send bar-nwc send-message 0 "HELLO OMGH!")