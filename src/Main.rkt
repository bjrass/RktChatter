(require (file "Common.rkt"))
(require (file "Client.rkt"))
(require (file "LocalClient.rkt"))
(require (file "NetworkClient.rkt"))
(require (file "Queue.rkt"))
(require (file "RsaEncoder.rkt"))
(require (file "Rsa.rkt"))

(sleep 3)
(display "Started\n")

; Create a client on localhost:10000
(define bar (new LocalClient% [port 10000]))
; Create a client on localhost:9000
(define foo (new LocalClient% [port 9000]))

(send bar set-username! "bar")
(send foo set-username! "foo")

; Tell one of the clients to start accepting incoming connections
(send foo start-accepting)
(send foo create-encryption!)
(send bar create-encryption!)
; Connect the two clients
(define bar-nwc (send bar connect "localhost" 9000))
; Wait for the connection attempt to finish
(send bar-nwc wait-for-connect)
; Tell foo to create a new encryption

; Start messaging!
;(send bar set-username! "I AM FOO!")
(send bar-nwc send-message "HELLO OMGH!")

(define (foomuch cnt)
  (define (iter i)
    (unless (= i 0)
      (send foo create-encryption!)
      (iter (- i 1))))
  (iter cnt))