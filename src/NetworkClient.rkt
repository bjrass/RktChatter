(module NetworkClient racket
  (require (file "Common.rkt"))
  (require (file "Client.rkt"))
  (require (file "Mutex.rkt"))
  (require (file "rsa.rkt"))
  
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
      (define m-should-listen #f)
      
      (define m-listener TRUE-NULL)
      (define m-listen-thread TRUE-NULL)
      (define m-is-connected #f)
      
      ; TODO Make encryption cleaner
      (define m-encryption #f)
      
      (define/public (get-encryption)
        m-encryption)
      
      (define/public (set-encryption! encryption)
        (set! m-encryption encryption))
      
      (define/public (encrypt-string string)
        (cond
          ((eq? m-encryption #f)
           string)
          
          (else
           (encrypt-string->list string m-encryption))))
      
      ;; Mutex for locking when this client is about to connect or disconnect
      (define m-conn-mut (make-mutex))
      
      ; Really wanted this to be private, but kind of a hassle because it needs to start a thread
      (define/public (connect-then-listen local-port)
        (set!-values (m-inport m-outport) (tcp-connect (get-hostname) (get-port) #f local-port))
        (listen-loop))
      
      ; Really wanted this to be private, but kind of a hassle because it needs to start a thread
      (define/public (listen-loop)
        ; Is it okay to do this here, or might there slip
        ; something by me?
        (lock-mutex m-conn-mut)
        (set! m-is-connected #t)
        (unlock-mutex m-conn-mut)
        
        (unless (TRUE-NULL? m-listener)
          (send m-listener client-connect this))
        
        (define (loop)
          (when m-should-listen
            (if (sync/timeout 0.1 m-inport)
                (let ((message (read m-inport)))
                  (unless (eof-object? message)
                    (unless (TRUE-NULL? m-listener)
                      (send m-listener client-message
                            (car message) (cdr message) this))
                    (loop)))
                (loop))))
        (loop)
        
        (lock-mutex m-conn-mut)
        (tcp-abandon-port m-inport)
        (tcp-abandon-port m-outport)
        (set! m-listen-thread TRUE-NULL)
        (set! m-is-connected #f)
        (unlock-mutex m-conn-mut)
        
        (unless (TRUE-NULL? m-listener)
          (send m-listener client-disconnect this)))
      
      (define/public (set-listener! listener)
        (set! m-listener listener))
      
      (define/public (is-connected?)
        (let ((ret #f))
          (lock-mutex m-conn-mut)
          (set! ret m-is-connected)
          (unlock-mutex m-conn-mut)
          ret))
      
      (define/public (connect (local-port #f))
        (unless (is-connected?)
          (set! m-should-listen #t)
          (set! m-listen-thread (thread (lambda () (send this connect-then-listen local-port))))))
      
      (define/public (accept-connect in out)
        (unless (is-connected?)
          (set! m-inport in)
          (set! m-outport out)
          (set! m-should-listen #t)
          (set! m-listen-thread (thread (lambda () (send this listen-loop))))))
      
      (define/public (request-disconnect)
        (set! m-should-listen #f))
      
      (define/public (disconnect)
        (when (is-connected?)
          (set! m-should-listen #f)
          (thread-wait m-listen-thread)))
      
      (define/public (send-message role data (encrypted #t))
        (let ((message #f))
          (if encrypted
              (set! message (encrypt-string data))
              (set! message data))
          
          (when (is-connected?) 
            (write (cons role message) m-outport)
            (flush-output m-outport))))
      
      ))(provide NetworkClientListener<%> NetworkClient%))