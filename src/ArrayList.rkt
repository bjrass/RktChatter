(module ArrayList racket
  (require (file "Common.rkt"))
  
  (define ArrayList%
    (class object%
      (super-new)
      
      (define m-sz-inc 5)
      (define m-sz-dec 10)
      
      (define m-size 0)
      (define m-vector (make-vector m-sz-inc))
      
      (define (paste-contents a b)
        (define (iter i)
          (when (and (< i (vector-length a)) (< i (vector-length b)))
            (vector-set! a i (vector-ref b i))
            (iter (+ i 1))))
        (iter 0))
      
      (define/private (prepare-insert pos)
        (let ((old m-vector))
          (when (= m-size (vector-length m-vector))
            (set! m-vector (make-vector (+ (vector-length m-vector) m-sz-inc)))
            (paste-contents m-vector old))
          
          (define (iter i)
            (when (>= i pos)
              (vector-set! m-vector (+ i 1) (vector-ref old i))
              (iter (- i 1))))
          (iter (- m-size 1))
          (set! m-size (+ m-size 1))))
      
      (define/public (insert! pos value)
        (prepare-insert pos)
        (vector-set! m-vector pos value))
      
      (define/public (append! value)
        (insert! m-size value))
      
      (define/public (set-at! pos value)
        (vector-set! m-vector pos value))
      
      (define/public (get-at pos)
        (vector-ref m-vector pos))
      
      (define/public (remove-at pos)
        (define (iter i)
          (when (< (+ i 1) m-size)
            (vector-set! m-vector i (vector-ref m-vector (+ i 1)))
            (iter (+ i 1))))
        (iter pos)
        (set! m-size (- m-size 1))
        (when (>= (- (vector-length m-vector) m-size) m-sz-dec)
          (let ((old m-vector))
            (set! m-vector (make-vector m-size))
            (paste-contents m-vector old))))
      
      (define/public (remove-value val [equality eq?])
        (define (iter i)
          (cond
            ((< i 0)
             #f)
            
            ((equality val (get-at i))
             (remove-at i)
             #t)
            
            (else
             (iter (- i 1)))))
        
        (define (while-rem)
          (cond
            ((iter (- (get-size) 1))
             (while-rem)
             #t)
            
            (else
             #f)))
        (while-rem))
      
      (define/public (get-size)
        m-size)))
  
  (provide ArrayList%))