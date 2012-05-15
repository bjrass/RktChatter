(require (file "Common.rkt"))
(require (file "Client.rkt"))
(require (file "LocalClient.rkt"))
(require (file "NetworkClient.rkt"))
(require (file "Queue.rkt"))
(require (file "RsaEncoder.rkt"))
(require (file "Rsa.rkt"))
(require (file "GuiClientInterface.rkt"))

(define foo (new GuiClientInterface% [port 10000]))
(define bar (new GuiClientInterface% [port 9000]))

(send foo set-username! "Foo")
(send bar set-username! "Bar")

(send bar grab-debug-output)

(send bar start-accepting)
(send foo start-accepting)
