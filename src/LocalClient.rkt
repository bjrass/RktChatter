(module LocalClient racket
  (require (file "Common.rkt"))
  (require (file "Debug.rkt"))
  (require (file "Mutex.rkt"))
  (require (file "Client.rkt"))
  (require (file "NetworkClient.rkt"))
  (require (file "Map.rkt"))
  (require (file "Rsa.rkt"))
  (require (file "RsaCodec.rkt"))
  ; TODO Make encryption cleaner
  ; TODO This class is veery thread-unsafe
  
  ;; \brief
  ;; A Client% for the messaging service running on the local computer.
  ;; 
  ;; The LocalClient% macro-manages connecting and messaging to other Clients.
  (define LocalClient%
    (class* Client% (NetworkClientListener<%>)
      (super-new [hostname "localhost"])
      
      (inherit get-port)
      (inherit get-username)
      
      ;; Map of all clients that are connected or is connecting
      ; TODO A problem is, a host can have multiple aliases
      (define m-connections (new Map%))
      (define m-conc-mut (make-mutex))
      
      (define m-accp-mut (make-mutex))
      (define STATE_IDLE 0)
      (define STATE_ACCEPTING 1)
      (define STATE_STOP 2)
      (define m-accept-state STATE_IDLE)
      
      (define m-accept-thread TRUE-NULL)
      (define m-tcplistener TRUE-NULL)
      
      (define m-codec (new RsaCodec%))
      
      (define/public (get-codec)
        m-codec)
      
      (define/public (create-encryption!)
        (send m-codec set-keys! (Rsa:make-keys))
        
        (define (iter next)
          (unless (send next finished?)
            (send (send next value) request-codec-update (lambda () (send m-codec copy)))))
        (iter (send m-connections get-iterator)))
      
      (define/private (accept)
        (let*-values (((inport outport) (tcp-accept m-tcplistener))
                      ((myhost myport-no host port-no) (tcp-addresses outport #t))
                      ((client) (new NetworkClient% [codec m-codec] [hostname host] [port port-no])))
          (send m-connections put! (cons host port-no) client)
          (send client set-listener! this)
          (send client accept-connect inport outport)))
      
      ; Would preferrably be made private in some way, though it must be started by a new thread so...
      (define (accept-loop)
        (define (loop)
          (sync/timeout 0.1 m-tcplistener)
          (when (= m-accept-state STATE_ACCEPTING)
            (when (tcp-accept-ready? m-tcplistener)
              (accept))
            (loop)))
        (loop)
        
        (lock-mutex m-accp-mut)
        (set! m-accept-thread TRUE-NULL)
        (tcp-close m-tcplistener)
        (set! m-tcplistener TRUE-NULL)
        (set! m-accept-state STATE_IDLE)
        (unlock-mutex m-accp-mut))
      
      (define/public (client-connect client)
        (when (string? (send this get-username))
          (send client send-message (send this get-username) 1)))
      
      ;(display* "Connected to " (send client get-hostname) " "
      ;          (send client get-port) "\n"))
      
      (define/public (client-disconnect client)
        (Dbg:feedback (list "LocalClient " "\"" (get-username) "\"")
                      "client-disconnect" (send client get-hostname) ":" (send client get-port)))
      
      (define/public (client-connect-fail client exceptions)
        (Dbg:feedback (list "LocalClient " "\"" (get-username) "\"")
                      "client-connect-fail" (send client get-hostname) ":" (send client get-port) "\n..." exceptions))
      
      (define/public (client-message client role args)
        (cond
          ; Message sent by client
          ((= role 0)
           (display* (send client get-username) " says: " args)
           (newline))
          
          ; Set username of client
          ((= role 1)
           (Dbg:feedback (list "LocalClient " "\"" (get-username) "\"")
                         "client-message" "Setting username of " (send client get-hostname) ":" (send client get-port))
           (send client set-username! args))
          
          (else
           (Dbg:feedback (list "LocalClient " "\"" (get-username) "\"")
                         "client-message" "Unknown role message from " (send client get-hostname) ":" (send client get-port)))))
      
      (define/public (connect host port-no)
        (let ((key (cons host port-no)))
          (unless (send m-connections has? key)
            (let ((client (new NetworkClient% [codec (send m-codec copy)] [hostname host] [port port-no])))
              (send m-connections put! key client)
              (send client set-listener! this)
              (send client connect #f)))
          (send m-connections find key)))
      
      (define/public (is-accepting?) 
        (= m-accept-state STATE_ACCEPTING))
      
      (define/public (start-accepting)
        (lock-mutex m-accp-mut)
        (when (= m-accept-state STATE_IDLE)
          (set! m-accept-state STATE_ACCEPTING)
          (set! m-tcplistener (tcp-listen (get-port) 10 #t))
          (set! m-accept-thread (thread (lambda () (accept-loop)))))
        (unlock-mutex m-accp-mut))
      
      (define/public (stop-accepting)
        (lock-mutex m-accp-mut)
        (cond
          ((not (= m-accept-state STATE_IDLE))
           (set! m-accept-state STATE_STOP)
           (unlock-mutex m-accp-mut)
           (thread-wait m-accept-thread))
          
          (else
           (unlock-mutex m-accp-mut))))
      
      (define/public (broadcast message [role 0] (encrypted #t))
        (define (iter next)
          (unless (send next finished?)
            (send (send next value) send-message message role encrypted)
            (iter (send next next))))
        (iter (send m-connections get-iterator)))
      
      (define/override (set-username! name)
        (super set-username! name)
        (when (string? name)
          (broadcast name 1)))
      
      (define/public (disconnect-all)
        (define (iter next)
          (unless (send next finished?)
            (send (send next value) request-disconnect)))
        (iter (send m-connections get-iterator)))
      
      ))(provide LocalClient%))
