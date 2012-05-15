(module LocalClient racket
  (require (file "Common.rkt"))
  (require (file "Debug.rkt"))
  (require (file "Mutex.rkt"))
  (require (file "Client.rkt"))
  (require (file "NetworkClient.rkt"))
  (require (file "ArrayList.rkt"))
  (require (file "Rsa.rkt"))
  (require (file "RsaCodec.rkt"))
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
      
      ;; List of all clients that are connected or is connecting
      (define m-connections (new ArrayList%))
      (define m-conn-mut (make-mutex))
      
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
      
      ;; Creates a new encryption with randomized keys
      ;; and notifies all connected clients
      (define/public (create-encryption!)
        (send m-codec set-keys! (Rsa:make-keys))
        
        (define (iter i)
          (when (>= i 0)
            (send (send m-connections get-at i)
                  request-codec-update (lambda () (send m-codec copy)))
            (iter (- i 1))))
        (lock-mutex m-conn-mut)
        (iter (- (send m-connections get-size) 1))
        (unlock-mutex m-conn-mut))
      
      (define/private (accept)
        (let*-values (((inport outport) (tcp-accept m-tcplistener))
                      ((myhost myport-no host port-no) (tcp-addresses outport #t))
                      ((client) (new NetworkClient% [codec m-codec] [hostname host] [port port-no])))
          (lock-mutex m-conn-mut)
          (send m-connections append! client)
          (unlock-mutex m-conn-mut)
          (send client set-listener! this)
          (send client accept-connect inport outport)
          
          ; TODO See definition of clients-changed
          (clients-changed)))
      
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
        (Dbg:feedback (list "LocalClient \"" (get-username) "\"")
                      "client-connect" (send client get-hostname) ":" (send client get-port))
        (let ((un (get-username)))
          (when (string? un)
            (send client send-message un 1))))
      
      (define/public (client-disconnect client)
        (Dbg:feedback (list "LocalClient \"" (get-username) "\"")
                      "client-disconnect" (send client get-hostname) ":" (send client get-port)))
      
      (define/public (client-connect-fail client exceptions)
        (Dbg:feedback (list "LocalClient \"" (get-username) "\"")
                      "client-connect-fail" (send client get-hostname) ":" (send client get-port) "\n..." exceptions))
      
      (define/public (client-message client role args)
        (cond
          ; Message sent by client
          ((= role 0)
           (let ((un (send client get-username)))
             (when (TRUE-NULL? un)
               (set! un "<noname>"))
             (send client set-log (string-append (send client get-log) un ":  " args "\n"))
             (log-update client)))
          
          ; Set username of client
          ((= role 1)
           (Dbg:feedback (list "LocalClient \"" (get-username) "\"")
                         "client-message" "Setting username of " (send client get-hostname) ":" (send client get-port))
           (send client set-username! args))
          
          (else
           (Dbg:feedback (list "LocalClient \"" (get-username) "\"")
                         "client-message" "Unknown role message from " (send client get-hostname) ":" (send client get-port)))))
      
      ; TODO Implemented mostly for short-term simplicity
      (define/public (clients-changed)
        #f)
      
      ; TODO Implemented mostly for short-term simplicity
      (define/public (log-update client)
        #f)
      
      (define/public (get-client num)
        (let ((client #f))
          (lock-mutex m-conn-mut)
          (set! client (send m-connections get-at num))
          (unlock-mutex m-conn-mut)
          client))
      
      (define/public (get-client-count)
        (let ((count #f))
          (lock-mutex m-conn-mut)
          (set! count (send m-connections get-size))
          (unlock-mutex m-conn-mut)
          count))
      
      (define/public (has-client? client)
        (let ((ret #f))
          (lock-mutex m-conn-mut)
          (set! ret (send m-connections has-value? client))
          (unlock-mutex m-conn-mut)
          ret))
      
      (define/public (connect host port-no)
        (let ((client (new NetworkClient% [codec (send m-codec copy)] [hostname host] [port port-no])))
          (lock-mutex m-conn-mut)
          (send m-connections append! client)
          (unlock-mutex m-conn-mut)
          
          (send client set-listener! this)
          (send client connect #f)
          
          ; TODO See definition of clients-changed
          (clients-changed)
          client))
      
      (define/public (drop client)
        (lock-mutex m-conn-mut)
        (when (send m-connections remove-value! client)
          (send client request-disconnect))
        (unlock-mutex m-conn-mut)
        ; TODO See definition of clients-changed
        (clients-changed))
      
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
        (define (iter i)
          (when (>= i 0)
            (send (send m-connections get-at i) send-message message role encrypted)
            (iter (- i 1))))
        (lock-mutex m-conn-mut)
        (iter (- (send m-connections get-size) 1))
        (unlock-mutex m-conn-mut))
      
      (define/override (set-username! name)
        (super set-username! name)
        (when (string? name)
          (broadcast name 1)))
      
      (define/public (disconnect-all)
        (define (iter i)
          (when (>= i 0)
            (send (send m-connections get-at i) request-disconnect)
            (iter (- i 1))))
        (lock-mutex m-conn-mut)
        (iter (- (send m-connections get-size) 1))
        (unlock-mutex m-conn-mut))
      
      ))(provide LocalClient%))
