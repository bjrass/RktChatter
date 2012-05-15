;; Contains some useful stuff that is
;; used in most other definitions
(module Common racket
  
  ;; Symbol meant to replace racket's inadequate null
  ;; The problem is that, instead of the empty list
  ;; being a list structure with nulled fields, it
  ;; is instead null.
  
  ;; This is due to the fact that the list is not really
  ;; a type, it is a concept of pairs being linked together
  ;; and terminated with null. Since we pretty much have to
  ;; use that concept, and since it assigns a global meaning to
  ;; null, null is no longer meaningless, it is the empty list.
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