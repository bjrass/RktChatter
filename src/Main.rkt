(require (file "Common.rkt"))
(require (file "Client.rkt"))
(require (file "LocalClient.rkt"))
(require (file "NetworkClient.rkt"))
(require (file "Queue.rkt"))
(require (file "RsaEncoder.rkt"))
(require (file "Rsa.rkt"))
(require (file "GuiClientInterface.rkt"))

(define bar (new GuiClientInterface% [port 9000]))

(send bar set-username! "Bar")

(send bar grab-debug-output)

(send bar start-accepting)
