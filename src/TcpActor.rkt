;; Intended to simulate tcp connections for debugging purposes
;; Not finished because the bugs in question was resolved

(require (file "Event.rkt"))
(require (file "Mutex.rkt"))
(require (file "Map.rkt"))

(define (tcpactor:make-pipe) (mcons (make-event) (mcons (make-mutex) (mcons null null))))
(define (tcpactor:pipe-event pipe) (mcar pipe))
(define (tcpactor:pipe-mutex pipe) (mcar (mcdr pipe)))
(define (tcpactor:pipe-port pipe) (mcdr (mcdr pipe)))

(define (tcpactor:pipe-read-link pipe)
  (define (loop rem)
    (if (null? (mcdr (mcdr rem)))
        rem
        (loop (mcdr rem))))
  
  (let ((port (tcpactor:pipe-port pipe)))
    (if (null? (mcdr port))
        null
        (loop port))))

(define (tcpactor:read pipe)
  (let ((mutex (tcpactor:pipe-mutex pipe)) (event (tcpactor:pipe-event pipe))
                                           (link #f) (ret #f))
    (define (block)
      (lock-mutex mutex)
      (set! link (tcpactor:pipe-read-link pipe))
      (when (null? link)
        (unlock-mutex mutex)
        (wait-event event)
        (block)))
    (block)
    
    (set! ret (mcar (mcdr link)))
    (set-mcdr! link null)
    (unlock-mutex mutex)
    ret))

(define (tcpactor:write data pipe)
  (let ((mutex (tcpactor:pipe-mutex pipe)) (event (tcpactor:pipe-event pipe)) (port (tcpactor:pipe-port pipe)))
    (lock-mutex mutex)
    (set-mcdr! port (mcons data (mcdr port)))
    (signal-event event)
    (unlock-mutex mutex)))

(define *listeners* (new Map%))

(define (tcpactor:tcp-listen port max-allow-wait reuse?)
  (let ((listener (mcons port (mcons (make-event) (mcons (make-vector max-allow-wait) 0)))))
    (send *listeners* put! port listener)
    listener))

(define (tcpactor:listener-port listener)
  (mcar listener))

(define (tcpactor:listener-event listener)
  (mcar (mcdr listener)))

(define (tcpactor:listener-vector listener)
  (mcar (mcdr (mcdr listener))))

(define (tcpactor:listener-num-wait listener)
  (mcdr (mcdr (mcdr listener))))

(define (tcpactor:listener-set-num-wait listener num)
  (set-mcdr! (mcdr (mcdr listener)) num))

(define (tcp-connect host port [localhost #f] [localport #f])
  (


#|
      
      (define *listeners* '
        
        (provide tcp-listen tcp-accept tcp-connect tcp-abandon-port tcp-addresses tcp-close tcp-accept-ready? sync/timeout))|#