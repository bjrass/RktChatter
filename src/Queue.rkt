(module Queue racket
  (require (file "Common.rkt"))
  
  (define Queue%
    (class object%
      (super-new)
      (define m-list null)
      
      (define/public (empty?)
        (null? m-list))
      
      (define/public (push! value)
        (set! m-list (mcons value m-list)))
      
      (define/public (get-front)
        (define (iter remn)
          (if (null? (mcdr remn))
              (mcar remn)
              (iter (mcdr remn))))
        (if (null? m-list)
            TRUE-NULL
            (iter m-list)))
      
      (define/public (pop!)
        (define (iter remn)
          (if (null? (mcdr (mcdr remn)))
              (set-mcdr! remn null)
              (iter (mcdr remn))))
        
        (unless (null? m-list)
          (if (null? (mcdr m-list))
              (set! m-list null)
              
              (iter m-list))))
      
      (define/public (clear!)
        (set! m-list null))))
  (provide Queue%))