(module Common racket
  ;; Symbol mean't to replace racket's inadequate null
  ;; The main reason why it is inadequate is that it
  ;; overlaps it's definition with the definition of an
  ;; empty list.
  (define TRUE-NULL 'NULL)
  
  (define (TRUE-NULL? something)
    (eq? something TRUE-NULL))
  
  (define (display* . args)
    (define (iter remn)
      (unless (null? remn)
        (display (car remn))
        (iter (cdr remn))))
    (iter args))
  
  (define (equal-values? a b)
    (cond
      ((and (pair? a) (pair? b))
       (and (equal-values? (car a) (car b)) (equal-values? (cdr a) (cdr b))))
      
      ((and (mpair? a) (mpair? b))
       (and (equal-values? (mcar a) (mcar b)) (equal-values? (mcdr a) (mcdr b))))
      
      (else (equal? a b))))
  
  (provide TRUE-NULL TRUE-NULL? display* equal-values?))