(module NetworkClient racket
  (require (file "Common.rkt"))
  (require (file "Mutex.rkt"))
  (require (file "Event.rkt"))
  (require (file "Client.rkt"))
  (require (file "RsaEncoder.rkt"))
  
  ; This client must have a lock-client and unlock-client state
  ; At first, the client is locked
  ; Then, public keys, username etc... are sent
  ; Then, unlock is sent
  ; Now the client can operate...
  ; Some time, lock-client will be sent
  ; Once the client is locked, send back client-locked
  ; Now, wait for unlock etc... (it is safe to exchange keys during this time)
  ; After the unlock, messages buffered during the locked time will be sent
  
  ;; \brief
  ;; Interface for processing messages from a NetworkClient%
  (define NetworkClientListener<%> (interface () client-message client-connect client-disconnect client-connect-fail))
  
  ;; \brief
  ;; A Client% that is, or can be, connected to
  (define NetworkClient%
    (class Client%
      (super-new)
      
      (inherit get-hostname)
      (inherit get-port)
      
      (define m-inport TRUE-NULL)
      (define m-outport TRUE-NULL)
      
      (define m-listener TRUE-NULL)
      (define m-listen-thread TRUE-NULL)
      
      ; 0: Idle NOT CONNECTED
      ; 1: Connect requested NOT CONNECTED
      ; 2: Connection established CONNECTED
      ; 3: Connection closing NOT CONNECTED
      (define m-conn-mut (make-mutex))
      (define m-conn-evt (make-event))
      (define STATE_IDLE 0)
      (define STATE_INIT 1)
      (define STATE_UP 2)
      (define STATE_CLOSE 3)
      (define m-connect-state STATE_IDLE)
      
      (define m-encoding (new RsaEncoder%))
      
      (define/private (set-state new-state)
        (set! m-connect-state new-state)
        (signal-event m-conn-evt))
      
      (define/public (get-encoding)
        m-encoding)
      
      (define/public (set-encoding! encoding)
        (send m-encoding unpack encoding))
      
      ; ############################
      ; ======= MESSAGE LOOP =======
      ; ############################
      
      ; Before connecting, the clients must exchange intial information about each other, most
      ; importantly encoding information.
      (define/private (exchange-client-info)
        'null)
      
      ; Processes a message sent by this client (or, rather, sent by the client it represents in the network)
      (define/private (process-message role message)
        (unless (TRUE-NULL? m-listener)
          (send m-listener client-message
                role message this)))
      
      ; Would preferrably be made private in some way, though it must be started by a new thread so...
      (define/public (connect-then-listen local-port)
        (let ((exceptions '()))
          (with-handlers (((lambda (ex) #t) (lambda (ex) (set! exceptions (cons ex exceptions)))))
            (set!-values (m-inport m-outport) (tcp-connect (get-hostname) (get-port) local-port)))
          
          (cond
            ((null? exceptions)
             (listen-loop))
            
            (else
             (lock-mutex m-conn-mut)
             (set! m-listen-thread TRUE-NULL)
             (set-state STATE_IDLE)
             (unlock-mutex m-conn-mut)
             
             (unless (TRUE-NULL? m-listener)
               (send m-listener client-connect-fail this exceptions))))))
      
      
      ; Would preferrably be made private in some way, though it must be started by a new thread so...
      (define/public (listen-loop)
        (exchange-client-info)
        
        (lock-mutex m-conn-mut)
        (set-state STATE_UP)
        (unlock-mutex m-conn-mut)
        
        (unless (TRUE-NULL? m-listener)
          (send m-listener client-connect this))
        
        (define (loop)
          (let ((can-read (sync/timeout 0.2 m-inport)))
            (when (= m-connect-state STATE_UP)
              (if can-read
                  (let ((message (read m-inport)))
                    (unless (eof-object? message)
                      (process-message (car message) (cdr message))
                      (loop)))
                  (loop)))))
        (loop)
        
        (lock-mutex m-conn-mut)
        (tcp-abandon-port m-inport)
        (tcp-abandon-port m-outport)
        (set! m-listen-thread TRUE-NULL)
        (set-state STATE_IDLE)
        (unlock-mutex m-conn-mut)
        
        (unless (TRUE-NULL? m-listener)
          (send m-listener client-disconnect this)))
      
      ; ###################################
      ; ======= END OF MESSAGE LOOP =======
      ; ###################################
      
      (define/public (set-listener! listener)
        (set! m-listener listener))
      
      (define/public (is-connected?)
        (let ((ret #f))
          (lock-mutex m-conn-mut)
          (set! ret (= m-connect-state STATE_UP))
          (unlock-mutex m-conn-mut)
          ret))
      
      (define/public (connect [local-port #f])
        (lock-mutex m-conn-mut)
        (when (= m-connect-state STATE_IDLE)
          (set-state STATE_INIT)
          (set! m-listen-thread (thread (lambda () (send this connect-then-listen local-port)))))
        (unlock-mutex m-conn-mut))
      
      (define/public (accept-connect in out)
        (lock-mutex m-conn-mut)
        (when (= m-connect-state STATE_IDLE)
          (set-state STATE_INIT)
          (set! m-inport in)
          (set! m-outport out)
          (set! m-listen-thread (thread (lambda () (send this listen-loop)))))
        (unlock-mutex m-conn-mut))
      
      (define/public (wait-for-connect)
        (let ((solid-state #f))
          (define (loop)
            (lock-mutex m-conn-mut)
            (set! solid-state (or (= m-connect-state STATE_IDLE) (= m-connect-state STATE_UP)))
            (unlock-mutex m-conn-mut)
            (unless solid-state
              (wait-event m-conn-evt)
              (loop)))
          (loop)))
      
      (define/public (request-disconnect)
        (lock-mutex m-conn-mut)
        (unless (= m-connect-state STATE_IDLE)
          (set-state STATE_CLOSE))
        (unlock-mutex m-conn-mut))
      
      (define/public (disconnect)
        (let ((needs-wait #f))
          (lock-mutex m-conn-mut)
          (unless (= m-connect-state STATE_IDLE)
            (set-state STATE_CLOSE)
            (set! needs-wait #t))
          (unlock-mutex m-conn-mut)
          (when needs-wait
            (thread-wait m-listen-thread))))
      
      (define/public (send-message data [role 0] [encrypted #t])
        (when (is-connected?) 
          (let ((message #f))
            (if encrypted
                (set! message (send m-encoding encode data))
                (set! message data))
            
            (write (cons role message) m-outport)
            (flush-output m-outport))))
      
      ))(provide NetworkClientListener<%> NetworkClient%))
