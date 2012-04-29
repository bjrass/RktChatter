(module RsaCodec racket
  (require (file "Common.rkt"))
  (require (file "Rsa.rkt"))
  (require (file "RsaEncoder.rkt"))
  
  (define RsaCodec%
    (class object%
      (super-new)
      
      (define m-private-key TRUE-NULL)
      (define m-public-key TRUE-NULL)
      
      (define/public (get-private-key)
        m-private-key)
      
      (define/public (get-public-key)
        m-public-key)
      
      (define/public set-keys!
        (case-lambda ([keys]
                      (set! m-private-key (Rsa:get-private-key keys))
                      (set! m-public-key (Rsa:get-public-key keys)))
                     ([public private]
                      (set! m-private-key (Rsa:get-private-key private))
                      (set! m-public-key (Rsa:get-public-key public)))))
      
      (define/public (get-encoder)
        (new RsaEncoder% [public-key m-public-key]))
      
      (define/public (get-packed-encoder)
        (send (new RsaEncoder% [public-key m-public-key]) pack))
      
      (define/public (decode data)
        (if (TRUE-NULL? m-private-key)
            data
            (Rsa:decrypt-list->string data m-private-key)))))
  
  (provide RsaCodec%))
