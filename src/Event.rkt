(module Event racket
  
  (require (file "Mutex.rkt"))
  
  (define (make-event)
    (mcons 0 (cons (make-mutex) (make-semaphore 0))))
  
  (define (wait-event event (timeout #f))
    (lock-mutex (car (mcdr event)))
    (set-mcar! event (+ (mcar event) 1))
    (unlock-mutex (car (mcdr event)))
    (sync/timeout timeout (cdr (mcdr event)))
    (lock-mutex (car (mcdr event)))
    (set-mcar! event (- (mcar event) 1))
    (unlock-mutex (car (mcdr event))))
  
  (define (signal-event event)
    (lock-mutex (car (mcdr event)))
    (define (loop posts)
      (unless (= posts 0)
        (semaphore-post (cdr (mcdr event)))
        (loop (- posts 1))))
    (loop (mcar event))
    (unlock-mutex (car (mcdr event))))
  
  (provide make-event wait-event signal-event))
