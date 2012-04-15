(module LocalClient racket
  (require (file "Common.rkt"))
  (require (file "Mutex.rkt"))
  (require (file "Client.rkt"))
  (require (file "NetworkClient.rkt"))
  (require (file "Map.rkt"))
  
  ;; \brief
  ;; A Client% for the messaging service running on the local computer.
  ;; 
  ;; The LocalClient% macro-manages connecting and messaging to other Clients.
  (define LocalClient%
    (class* Client% (NetworkClientListener<%>)
      (super-new (hostname "localhost"))
      
      (inherit get-port)
      
      ;; Map of all clients that are connecting, is connected etc
      (define m-connections (new Map%))
      (define m-conc-mut (make-mutex))
      
      (define m-should-accept #f)
      (define m-is-accepting #f)
      (define m-accept-thread TRUE-NULL)
      (define m-tcplistener TRUE-NULL)
      (define m-accp-mut (make-mutex))
      
      (define/private (accept)
        (let*-values (((inport outport) (tcp-accept m-tcplistener))
                      ((myhost myport-no host port-no) (tcp-addresses outport #t))
                      ((client) (new NetworkClient% [hostname host] [port port-no])))
          (send m-connections put! (cons host port-no) client)
          (send client set-listener! this)
          (send client accept-connect inport outport)))
            
      ; Really wanted this to be private, but kind of a hassle because it needs to start a thread
      (define/public (accept-loop)
        ; Okay to do here?
        (lock-mutex m-accp-mut)
        (set! m-is-accepting #t)
        (unlock-mutex m-accp-mut)
        
        (define (loop)
          (when m-should-accept
            (accept)
            (loop)))
        (loop)
                
        (lock-mutex m-accp-mut)
        (set! m-accept-thread TRUE-NULL)
        (tcp-close m-tcplistener)
        (set! m-tcplistener TRUE-NULL)
        (set! m-is-accepting #f)
        (unlock-mutex m-accp-mut))
      
      (define/public (client-connect client)
        (display* "Connected to " (send client get-hostname) " " (send client get-port) "\n"))
      
      (define/public (client-disconnect client)
        (display* "Disconnected from " (send client get-hostname) " " (send client get-port) "\n"))
      
      (define/public (client-connect-fail client)
        (display* "Failed to connect to " (send client get-hostname) " " (send client get-port) "\n"))
      
      (define/public (client-message message client)
        (display* (send client get-username) " Message: " message "\n"))
      
      (define/public (connect host port-no)
        (let ((key (cons host port-no)))
          (unless (send m-connections has? key)
            (let ((client (new NetworkClient% [hostname host] [port port-no])))
              (send m-connections put! key client)
              (send client set-listener! this)
              (send client connect (get-port))))
          (send m-connections find key)))
      
      (define/public (is-accepting?)
        (let ((ret #f))
          (lock-mutex m-accp-mut)
          (set! ret m-is-accepting)
          (unlock-mutex m-accp-mut)
          ret))
      
      (define/public (start-accepting)
        (unless (is-accepting?)
          (set! m-should-accept #t)
          (set! m-tcplistener (tcp-listen (get-port) 10 #t))
          (set! m-accept-thread (thread (lambda () (send this accept-loop))))))
      
      (define/public (stop-accepting)
        (when (is-accepting?)
          (set! m-should-accept #f)
          (thread-wait m-accept-thread)))
      
      ))(provide LocalClient%))

; Internals shouldn't use is-accepting?, they should lock the mutex until they are done.