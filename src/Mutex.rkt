;; \brief
;; Implemented by using a semaphore, but
;; using mutexes will be so much more common
;; in this project.
(module Mutex racket
  (define (make-mutex) (cons 'mutex (make-semaphore 1)))
  (define (lock-mutex mut) (semaphore-wait (cdr mut)))
  (define (unlock-mutex mut) (semaphore-post (cdr mut)))
  (define (is-mutex? mut) (and (pair? mut) (eq? (car mut) 'mutex) (semaphore? (cdr mut))))
  
  (provide make-mutex lock-mutex unlock-mutex is-mutex?))