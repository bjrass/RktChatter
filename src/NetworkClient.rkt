(module NetworkClient racket
  (require (file "Common.rkt"))
  (require (file "Debug.rkt"))
  (require (file "Mutex.rkt"))
  (require (file "Event.rkt"))
  (require (file "Client.rkt"))
  (require (file "RsaEncoder.rkt"))
  (require (file "Queue.rkt"))
  
  ;; Interface for processing messages from a NetworkClient%
  (define NetworkClientListener<%> (interface () client-message client-connect client-disconnect client-connect-fail))
  
  ;; A Client% that is, or can be, connected to
  (define NetworkClient%
    (class Client%
      (super-new)
      (init codec)
      
      (inherit get-hostname)
      (inherit get-port)
      
      (define ROLE_PUBLIC_KEY 0)
      (define ROLE_START_BUFFERING 1)
      (define ROLE_IS_BUFFERING 2)
      (define ROLE_STOP_BUFFERING 3)
      (define ROLE_USER 4)
      
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
      (define STATE_OPEN 1)
      (define STATE_UP 3)
      (define STATE_CLOSE 4)
      (define m-connect-state STATE_IDLE)
      
      ; The encoder for sending encoded messages to the remote end
      (define m-encoder (new RsaEncoder%))
      
      ; Sometimes, briefly, sending encoded messages
      ; will be disabled due to the encoding of a
      ; client is changing. During this time, messages
      ; will be buffered and sent afterwards instead.
      ; Although this only affects encoded messages, it
      ; does in turn affects all messages in order to
      ; perserve TCP properties.
      (define m-message-buffer TRUE-NULL)
      (define m-buffer-mut (make-mutex))
      (define m-buffering #f)
      
      ; The codec for decoding messages received by this client from the remote end
      (define m-codec (if (TRUE-NULL? codec)
                          TRUE-NULL
                          (send codec copy)))
      
      ; TODO
      ; This is kind of a temporary solution, the same with how the username is handeled.
      ; They should be defined in a later stage, through extending this otherwise
      ; basic networking class.
      (define m-log "")
      
      (define/public (get-log)
        m-log)
      (define/public (set-log log)
        (set! m-log log))
      
      ; Callback for updating the codec
      (define m-cb-codec TRUE-NULL)
      ; Flag telling whether the codec is being updated
      (define m-update-codec #f)
      (define m-codec-mut (make-mutex))
      
      (define/private (set-state new-state)
        (set! m-connect-state new-state)
        (signal-event m-conn-evt))
      
      (define/private (create-message encoded role data)
        (cons encoded (cons role data)))
      
      (define/private (dispatch-message message)
        (Dbg:feedback (list "NetworkClient " (get-hostname) ":"
                            (get-port)) "dispatch-message" message)
        (when (is-connected?)
          (let ((actual-message message))
            (when (car message)
              (set! actual-message
                    (cons (car message)
                          (cons (car (cdr message))
                                (send m-encoder encode (cdr (cdr message)))))))
            
            (write actual-message m-outport)
            (flush-output m-outport))))
      
      (define/private (start-buffering)
        (lock-mutex m-buffer-mut)
        (unless m-buffering
          (set! m-buffering #t)
          (set! m-message-buffer (new Queue%)))
        (unlock-mutex m-buffer-mut))
      
      (define/private (stop-buffering)
        (lock-mutexes m-conn-mut m-buffer-mut)
        (when m-buffering
          (set! m-buffering #f)
          (define (iter)
            (unless (send m-message-buffer empty?)
              (dispatch-message (send m-message-buffer get-front))
              (send m-message-buffer pop!)
              (iter)))
          (iter)
          (set! m-message-buffer TRUE-NULL))
        (unlock-mutexes m-conn-mut m-buffer-mut))
      
      (define/private (codec-update)
        (lock-mutex m-codec-mut)
        (when m-update-codec
          (set! m-update-codec #f)
          (set! m-codec (send (m-cb-codec) copy))
          (Dbg:feedback (list "NetworkClient " (get-hostname) ":"
                              (get-port)) "codec-update"
                                                    "pub: " (send m-codec get-public-key)
                                                    " priv: " (send m-codec get-private-key)))
        (unlock-mutex m-codec-mut))
      
      ;; Returns the codec related with this client
      ;; The codec can differ between client connections because
      ;; changing the codec needs to lock messaging, and
      ;; clients may lock at different times.
      (define/public (get-codec)
        m-codec)
      
      ;; Sets a listener to handle events for this client
      (define/public (set-listener! listener)
        (set! m-listener listener))
      
      ;; Returns whether this client is considered to be connected (ready to read, write)
      (define/public (is-connected?)
        (= m-connect-state STATE_UP))
      
      ;; Requests a change of codec, where get-codec-callback
      ;; will be used to get the new codec when the change is possible.
      (define/public (request-codec-update get-codec-callback)
        (lock-mutexes m-conn-mut m-codec-mut)
        (if (is-connected?)
            (begin
              (Dbg:feedback (list "NetworkClient " (get-hostname) ":"
                                  (get-port)) "request-codec-update" "while connected")
              (set! m-cb-codec get-codec-callback)
              (unless m-update-codec
                (set! m-update-codec #t)
                (dispatch-message (create-message #f ROLE_START_BUFFERING null))))
            
            ; In order for the following to be safe:
            ; The client must, after it has initiated communications with a client,
            ; and after it has sent its public key to the other end, be officially be
            ; considered connected. Otherwise, the codec could change locally but not
            ; on the remote end.
            (begin
              (Dbg:feedback (list "NetworkClient " (get-hostname) ":"
                                  (get-port)) "request-codec-update" "while disconnected")
              (set! m-codec (send (get-codec-callback) copy))))
        (unlock-mutexes m-conn-mut m-codec-mut))
      
      (define/public (get-encoder)
        m-encoder)
      
      (define/public (set-encoder! encoder)
        (send m-encoder unpack encoder))
      
      ; ############################
      ; ======= MESSAGE LOOP =======
      ; ############################
      
      ;; Processes a message sent by this client (or, rather, sent by the client it represents in the network)
      (define/private (process-message encoded role message)
        (when encoded
          (set! message (send m-codec decode message)))
        
        (if (>= role ROLE_USER)
            (unless (TRUE-NULL? m-listener)
              (Dbg:feedback (list "NetworkClient " (get-hostname) ":"
                                  (get-port)) "process-message" "user message " message)
              (send m-listener client-message this
                    (- role ROLE_USER) message))
            (cond
              ; Sets the public key (should only be used when the client is buffering)
              ((= role ROLE_PUBLIC_KEY)
               (Dbg:feedback (list "NetworkClient " (get-hostname) ":"
                                   (get-port)) "process-message" "public key " message)
               (unless m-buffering
                 (error "INTERNAL ERROR; PUBLIC KEY WHEN OUT OF BUFFERING MODE!"))
               (send m-encoder unpack message))
              
              ; Starts buffering and responds with a confirmation
              ((= role ROLE_START_BUFFERING)
               (Dbg:feedback (list "NetworkClient " (get-hostname) ":"
                                   (get-port)) "process-message" "start buffering")
               (start-buffering)
               (lock-mutex m-conn-mut)
               (dispatch-message (create-message #f ROLE_IS_BUFFERING null))
               (unlock-mutex m-conn-mut))
              
              ; Confirmation that the remote end of the client is now buffering
              ((= role ROLE_IS_BUFFERING)
               (Dbg:feedback (list "NetworkClient " (get-hostname) ":"
                                   (get-port)) "process-message" "is buffering")
               
               (lock-mutex m-conn-mut)
               (codec-update)
               (dispatch-message (create-message #f ROLE_PUBLIC_KEY (send m-codec get-packed-encoder)))
               (dispatch-message (create-message #f ROLE_STOP_BUFFERING null))
               (unlock-mutex m-conn-mut))
              
              ; Merely flushes the message queue and stops buffering
              ((= role ROLE_STOP_BUFFERING)
               (Dbg:feedback (list "NetworkClient " (get-hostname) ":"
                                   (get-port)) "process-message" "stop buffering")
               (stop-buffering)))))
      
      ;; Extension of the message loop, this also connects the client in the message thread
      (define (connect-then-listen local-port)
        (let ((exceptions '()))
          (with-handlers (((lambda (ex) #t) (lambda (ex) (set! exceptions (cons ex exceptions)))))
            (set!-values (m-inport m-outport) (tcp-connect (get-hostname) (get-port) #f local-port)))
          
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
      
      
      ;; Main loop for the connection process, listening for and reading input as well as handling cleanup
      (define (listen-loop)
        (lock-mutex m-conn-mut)
        (start-buffering)
        (set-state STATE_UP)
        
        (dispatch-message (create-message #f ROLE_PUBLIC_KEY (send m-codec get-packed-encoder)))
        (dispatch-message (create-message #f ROLE_STOP_BUFFERING null))
        (unlock-mutex m-conn-mut)
        
        (unless (TRUE-NULL? m-listener)
          (send m-listener client-connect this))
        
        (define (loop)
          (let ((can-read (sync/timeout 0.1 m-inport)))
            (when (= m-connect-state STATE_UP)
              (if can-read
                  (let ((message (read m-inport)))
                    (unless (eof-object? message)
                      (process-message (car message) (car (cdr message)) (cdr (cdr message)))
                      (loop)))
                  (loop)))))
        (loop)
        
        ; If we have already disconnected, we are free
        ; to update the codec locally (if we need it)
        (codec-update)
        
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
      
      ;; Attempts to connect to this client
      ;; Returns *immediately*, see "wait-for-connect"
      (define/public (connect [local-port #f])
        (lock-mutex m-conn-mut)
        (when (= m-connect-state STATE_IDLE)
          (set-state STATE_OPEN)
          (set! m-listen-thread (thread (lambda () (connect-then-listen local-port)))))
        (unlock-mutex m-conn-mut))
      
      ;; Accepts a connection initiated by this client's remote
      (define/public (accept-connect in out)
        (lock-mutex m-conn-mut)
        (when (= m-connect-state STATE_IDLE)
          (set-state STATE_OPEN)
          (set! m-inport in)
          (set! m-outport out)
          (set! m-listen-thread (thread (lambda () (listen-loop)))))
        (unlock-mutex m-conn-mut))
      
      ;; Waits for calls to connect and accept-connect to reach a conclusion
      ;; Either returns as the client has connected or when it is disconnected
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
      
      ;; Requests a disconnect of this NetworkClient%
      ;; This always returns *immediately* and the connection can
      ;; still be ongoing on return.
      (define/public (request-disconnect)
        (lock-mutex m-conn-mut)
        (unless (= m-connect-state STATE_IDLE)
          (set-state STATE_CLOSE))
        (unlock-mutex m-conn-mut))
      
      ;; Disconnects this NetworkClient% and waits for the disconnection process to finish
      ;; This is safe to call at any time, and will return only when the client is disconnected
      (define/public (disconnect)
        (let ((needs-wait #f))
          (lock-mutex m-conn-mut)
          (unless (= m-connect-state STATE_IDLE)
            (set-state STATE_CLOSE)
            (set! needs-wait #t))
          (unlock-mutex m-conn-mut)
          (when needs-wait
            (thread-wait m-listen-thread))))
      
      (define/public (send-message data [role 0] [encoded #t])
        (let ((message (create-message encoded (+ ROLE_USER role) data)))
          (lock-mutexes m-conn-mut m-buffer-mut)
          (if m-buffering
              (send m-message-buffer push! message)
              (dispatch-message message))
          (unlock-mutexes m-conn-mut m-buffer-mut)))
      
      ))(provide NetworkClientListener<%> NetworkClient%))
