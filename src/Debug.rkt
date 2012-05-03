(module Debug racket
  (require (file "Mutex.rkt"))
  
  (define dbg-disp-mut (make-mutex))
  
  (define Dbg:display-plain display)
  
  (define (Dbg:set-base-display disp)
    (set! Dbg:display-plain disp))
  
  (define (Dbg:flush)
    (flush-output))
  
  (define (Dbg:display-list list)
    (if (or (list? list) (pair? list))
        (unless (null? list)
          (Dbg:display-plain (car list))
          (Dbg:display-list (cdr list)))
        (Dbg:display-plain list)))
  
  (define (Dbg:display message)
    (lock-mutex dbg-disp-mut)
    (Dbg:display-plain message)
    (Dbg:flush)
    (unlock-mutex dbg-disp-mut))
  
  (define (Dbg:display* . args)
    (lock-mutex dbg-disp-mut)
    (Dbg:display-list args)
    (Dbg:flush)
    (unlock-mutex dbg-disp-mut))
  
  (define (Dbg:feedback section sub-section . message)
    (lock-mutex dbg-disp-mut)
    (Dbg:display-plain "<")
    (Dbg:display-list section)
    (Dbg:display-plain "> #")
    (Dbg:display-list sub-section)
    (Dbg:display-plain ": ")
    (Dbg:display-list message)
    (Dbg:display-plain "\n")
    (Dbg:flush)
    (unlock-mutex dbg-disp-mut))
     
     (provide Dbg:display Dbg:set-base-display Dbg:display* Dbg:feedback))