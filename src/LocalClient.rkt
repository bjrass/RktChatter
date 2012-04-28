(module LocalClient racket
  (require (file "Common.rkt"))
  (require (file "Mutex.rkt"))
  (require (file "Client.rkt"))
  (require (file "NetworkClient.rkt"))
  (require (file "Map.rkt"))
  (require (file "rsa.rkt"))
  ; TODO Make encryption cleaner
  
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
      
      ; TODO Make encryption cleaner
      ; TODO Send encryption on connect
      (define m-encryption #f)
      (define/public (create-encryption!)
        (set! m-encryption (make-keys))
        (broadcast 2 (get-public-key m-encryption) #f))
      (define/public (get-encryption)
        m-encryption)
      
      (define/public (decrypt-message message)
        (cond
          ((eq? m-encryption #f)
           message)
          
          (else (decrypt-list->string message (get-private-key m-encryption)))))
      
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
        (display* "Connected to " (send client get-hostname) " "
                  (send client get-port) "\n"))
      
      (define/public (client-disconnect client)
        (display* "Disconnected from " (send client get-hostname) " "
                  (send client get-port) "\n"))
      
      (define/public (client-connect-fail client)
        (display* "Failed to connect to " (send client get-hostname) " "
                  (send client get-port) "\n"))
      
      ; TODO Make encryption cleaner
      (define/public (client-message role args client)
        (cond
          ; Send message
          ((= role 0)
           (display* (send client get-username) " says: " (decrypt-message args))
           (newline))
          
          ; Set username
          ((= role 1)
           (send client set-username! (decrypt-message args)))
          
          ; Set public key
          ((= role 2)
           (send client set-encryption! args))
          
          (else
           (display* "Message of unknown role: " role " sent"))))
      
      (define/public (connect host port-no)
        (let ((key (cons host port-no)))
          (unless (send m-connections has? key)
            (let ((client (new NetworkClient% [hostname host] [port port-no])))
              (send m-connections put! key client)
              (send client set-listener! this)
              (send client connect (get-port))))
          (send m-connections find key)))
      
      ; \remark
      ; internals does not use this member as they should unlock the associated
      ; mutex first after they have have done what they intend to do.
      (define/public (is-accepting?) 
        (let ((ret #f))
          (lock-mutex m-accp-mut)
          (set! ret m-is-accepting)
          (unlock-mutex m-accp-mut)
          ret))
      
      (define/public (start-accepting)
        (lock-mutex m-accp-mut)
        (unless m-is-accepting
          (set! m-should-accept #t)
          (set! m-tcplistener (tcp-listen (get-port) 10 #t))
          (set! m-accept-thread (thread (lambda () (send this accept-loop)))))
        (unlock-mutex m-accp-mut))
      
      (define/public (stop-accepting)
        (lock-mutex m-accp-mut)
        (when m-is-accepting
          (set! m-should-accept #f)
          (thread-wait m-accept-thread))
        (unlock-mutex m-accp-mut))
      
           
      (define/public (broadcast role message (encrypted #t))
        (define (iter next)
          (unless (send next finished?)
            (send (send next value) send-message role message encrypted)
            (iter (send next next))))
        (iter (send m-connections getIterator)))
      
      ; TODO Send username on connect
      (define/override (set-username! name)
        (super set-username! name)
        (broadcast 1 name))
      
      ))(provide LocalClient%))
