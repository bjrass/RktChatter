;; \brief
;; The mutex is in fact a semaphore, but
;; most of the time mutexes are what's needed
(module Mutex racket
  (define (make-mutex)
    (make-semaphore 1))
  
  (define (lock-mutex mut [timeout #f])
    (sync/timeout timeout mut))
  (define (unlock-mutex mut)
    (semaphore-post mut))
  
  (define (lock-mutexes . mutexes)
    (define (iter remn)
      (unless (null? remn)
        (lock-mutex (car remn))
        (iter (cdr remn))))
    (iter mutexes))
  
  (define (unlock-mutexes . mutexes)
    (define (iter remn)
      (unless (null? remn)
        (unlock-mutex (car remn))
        (iter (cdr remn))))
    (iter mutexes))
  
  (provide make-mutex lock-mutex unlock-mutex lock-mutexes unlock-mutexes))
