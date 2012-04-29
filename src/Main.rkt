(require (file "Common.rkt"))
(require (file "Client.rkt"))
(require (file "LocalClient.rkt"))
(require (file "NetworkClient.rkt"))

; Create a client on localhost:10000
(define bar (new LocalClient% [port 10000]))
; Create a client on localhost:9000
(define foo (new LocalClient% [port 9000]))
; Tell one of the clients to start accepting incoming connections
(send foo start-accepting)
; Connect the two clients
(define bar-nwc (send bar connect "localhost" 9000))
; Wait for the connection attempt to finish
(send bar-nwc wait-for-connect)
; Tell foo to create a new encryption
; (send foo create-encryption!)

; Wait (Locks must be applied...)
; (sleep 1)

; Start messaging!
(send bar set-username! "I AM FOO!")
(send bar-nwc send-message "HELLO OMGH!")
