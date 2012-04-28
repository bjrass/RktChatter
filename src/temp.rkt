(module NetworkClient racket
  (require (file "Common.rkt"))
  (require (file "Client.rkt"))
  (require (file "Mutex.rkt"))
  
  ;; \brief
  ;; Interface for processing messages from a NetworkClient%
  (define NetworkClientListener<%> (interface () client-message client-connect client-disconnect client-connect-fail))
  
  ;; \brief
  ;; A Client% that is, or can be, connected to
  (define NetworkClient%
    (class Client%
      (super-new)
      
      (define m-listener TRUE-NULL)
      
      (define/private (listen-loop)
        (unless (TRUE-NULL? m-listener)
          (send m-listener client-connect this)))
      
      (define/public (send data)
        (when (is-connected?) 
          (write data m-outport)))
      
      ))(provide NetworkClientListener<%> NetworkClient%))