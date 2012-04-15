(require (file "Common.rkt"))
(require (file "Map.rkt"))
(require (file "Client.rkt"))
(require (file "LocalClient.rkt"))
(require (file "NetworkClient.rkt"))

(define foo (new LocalClient% [port 9000]))
(send foo start-accepting)
