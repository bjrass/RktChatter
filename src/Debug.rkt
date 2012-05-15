;; Provides some debug message utilities,
;; such as templated feedback functions
;; for uniform debug messages and mutexes
;; to prevent messages from different threads
;; to interfere with one another.
(module Debug racket
  (require (file "Mutex.rkt"))
  
  (define dbg-disp-mut (make-mutex))
  
  (define Dbg:display-plain display)
  
  ;; Sets the debugging output display, that is used by
  ;; all output functions.
  ;; Should take one argument, which is the item that should be displayed.
  (define (Dbg:set-base-display disp)
    (set! Dbg:display-plain disp))
  
  ;; Flushes the debug output. All debugging output
  ;; functions flushes the output before returning
  (define (Dbg:flush)
    (flush-output))
  
  (define (Dbg:display-list list)
    (if (or (list? list) (pair? list))
        (unless (null? list)
          (Dbg:display-plain (car list))
          (Dbg:display-list (cdr list)))
        (Dbg:display-plain list)))
  
  ;; Displays a message to the debug output
  (define (Dbg:display message)
    (lock-mutex dbg-disp-mut)
    (Dbg:display-plain message)
    (Dbg:flush)
    (unlock-mutex dbg-disp-mut))
  
  ;; Displays a number of arguments to the debug output
  (define (Dbg:display* . args)
    (lock-mutex dbg-disp-mut)
    (Dbg:display-list args)
    (Dbg:flush)
    (unlock-mutex dbg-disp-mut))
  
  ;; Displays some feedback through the debug output,
  ;; where the feedback's origin is displayed along with
  ;; the message.
  ;; A typical use for classes would be to
  ;; pass along the classname in 'section', the member name
  ;; in sub-section and then the actual feedback.
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