(module Client racket
  (require (file "Common.rkt"))
  
  ;; Abstract Client% implementation containing information
  ;; that is shared by Clients through their connections.
  (define Client%
    (class object%
      (super-new)
      
      (init hostname)
      (init [port 9000])
      (init [username TRUE-NULL])
      
      (define m-username username)
      (define m-hostname hostname)
      (define m-port port)
      
      (define/public (get-port)
        m-port)
      (define/public (get-hostname)
        m-hostname)
      (define/public (get-username)
        m-username)
      
      (define/public (set-port! port)
        (set! m-port port))
      (define/public (set-username! username)
        (set! m-username username))
      ; Setting hostname does not make sense
      
      ))(provide Client%))