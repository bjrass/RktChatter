;; \brief
;; The mutex is in fact a semaphore, but
;; most of the time mutexes are what's needed
(module Mutex racket
  (define (make-mutex) (make-semaphore 1))
  (define (lock-mutex mut (timeout #f)) (sync/timeout timeout mut))
  (define (unlock-mutex mut) (semaphore-post mut))
  
  (provide make-mutex lock-mutex unlock-mutex))
