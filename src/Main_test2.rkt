(require (file "Common.rkt"))
(require (file "Map.rkt"))
(require (file "Client.rkt"))
(require (file "LocalClient.rkt"))
(require (file "NetworkClient.rkt"))

(define bar (new LocalClient% [port 9000]))
(define foo-nwc (send bar connect "192.168.1.139" 9000))

; The clients actually aren't connected yet, there should either be a lock or a wait-for-connect

(sleep 3)

(send foo-nwc send-message "Ser du det har, fungerar det, spread the word")