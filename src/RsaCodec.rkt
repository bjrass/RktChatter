(module RsaCodec racket
  (require (file "Common.rkt"))
  (require (file "Rsa.rkt"))
  (require (file "RsaEncoder.rkt"))
  
  ;; Bundles everything used in a RSA-encryption
  (define RsaCodec%
    (class object%
      (super-new)
      
      (init [private-key TRUE-NULL])
      (init [public-key TRUE-NULL])
      
      (define m-private-key private-key)
      (define m-public-key public-key)
      
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
            (Rsa:decrypt-list->string data m-private-key)))
      
      (define/public (copy)
        (new RsaCodec% [private-key m-private-key] [public-key m-public-key]))))
  
  (provide RsaCodec%))
