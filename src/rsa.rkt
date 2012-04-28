(module RsaCodec racket
  (require (file "prime numbers.rkt"))
  
  ; returns all the prime factors to the number n, 
  ; if n's biggest factor is less then the biggest prime in "prime numbers.rkt"
  ; Example: (factor 100) -> (5 5 2 2)
  (define (factor n)
    (factor_help n '() (car list-of-prime-num) (cdr list-of-prime-num)))
  
  (define (factor_help n dividors cur-prime rest-primes)
    (cond
      ((= 0 n)
       dividors)
      
      ((= 1 (/ n cur-prime))
       (factor_help 0 (cons cur-prime dividors) 0 '()))
      
      ((= (/ n cur-prime) (floor(/ n cur-prime)))
       (factor_help (/ n cur-prime) (cons cur-prime dividors) cur-prime rest-primes))
      
      (else
       (if (eq? rest-primes '())
           (cons 'out-of-range dividors)
           (factor_help n dividors (car rest-primes) (cdr rest-primes))))))
  
  (define (return-element num list)
    (if (= num 1)
        (car list)
        (return-element (- num 1) (cdr list))))
  
  (define (rand-between a b)
    (+ a (random (- (+ b 1) a))))
  
  ; P and Q in form: (P Q)
  ; returns two random primes between 'low' and 'high' in list-of-prime-num
  (define (two-prime-num-between low high)
    (let ((p 0)(q 0)(rand 0))
      (begin
        (set! rand (rand-between low  (- high 1)))
        (set! p (return-element rand list-of-prime-num))
        (set! q (return-element (rand-between rand high) list-of-prime-num))
        (list p q))))
  
  (define (prime-bigger-than num)
    (prime-bigger-than_help num 2 list-of-prime-num))
  
  (define (prime-bigger-than_help num cur-prime list)
    (if (> cur-prime num)
        cur-prime
        (prime-bigger-than_help num (car list) (cdr list))))
  
  ;Makes E, m = (P-1)*(Q-1) 
  (define (make-ee m)
    (let ((factors 0))
      (begin
        (set! factors (factor m))
        (prime-bigger-than (+ (random 50) (car factors))))))
  
  ;Make D, m = (P-1)*(Q-1)      
  (define (make-d ee m)
    (make-d_help ee 1 m))
  
  (define (make-d_help ee d m)
    (if (= (/ 1 m) (- (/ (* ee d) m) (floor (/ (* ee d) m))))
        d
        (make-d_help ee (+ d 1) m)))
  
  ; (make-keys) -> ((public common) (private common))
  (define (make-keys)
    (let ((temp 0)(p 0)(q 0)(n 0)(m 0)(ee 0)(d 0))
      (begin
        (set! temp (two-prime-num-between 10 20)) ;higher a and b gives better keys but are slower to decrypt. (two-prime-num-between a b) (a < b)
        (set! p (car temp))
        (set! q (car (cdr temp)))
        (set! n (* p q))
        (set! m (* (- p 1) (- q 1)))
        (set! ee (make-ee m))
        (set! d (make-d ee m))
        (list (list ee n) (list d n)))))
  
  
  (define key (make-keys))
  (define pub (car key))
  (define priv (car (cdr key)))
  
  ; The highest possible number (message) to encrypt is 'n', ((pub n)(priv n))
  (define (encrypt m pub-key) (modulo (expt m (car pub-key)) (car (cdr pub-key))))
  
  (define (decrypt m priv-key) (modulo (expt m (car priv-key)) (car (cdr priv-key))))
  
  (define (diff-test diff list)
    (cond
      ((eq? list '())
       diff)
      
      ((> (- (car (cdr list)) (car list)) diff)
       (diff-test (- (car(cdr list)) (car list)) (cdr list)))
      
      (else
       (diff-test diff (cdr list)))))
  
  (define (make-true-keys)
    (let ((key (make-keys))(pub '()) (priv '()))
      (begin
        (set! pub (car key))
        (set! priv (car (cdr key)))
        (if (= 1234 (decrypt (encrypt 1234 priv) pub))
            key
            (make-true-keys)))))
  
  (define (key-test m public private)
    (when (>= 10000 m)
      (display (- m (decrypt (encrypt m public) private)))
      (display " - ")
      (display m)
      (display " - ")
      (display (encrypt m public))
      (display " - ")
      (display (decrypt (encrypt m public) private))
      
      (display "\n")
      (key-test (+ m 1) public private)))
  
  (define (encrypt-list message pub-key)
    (encrypt-list_help message pub-key '()))
  
  (define (encrypt-list_help message pub-key crypt)
    (if (eq? message '())
        crypt
        (encrypt-list_help (cdr message) pub-key (cons (encrypt (car message) pub-key) crypt))))
  
  
  (define (decrypt-list crypt priv-key)
    (decrypt-list_help crypt priv-key '()))
  
  (define (decrypt-list_help crypt priv-key message)
    (if (eq? crypt '())
        message
        (decrypt-list_help (cdr crypt) priv-key (cons (decrypt (car crypt) priv-key) message))))  
  
  
  ;((521 2697293) (2647097 2697293))  
  ;(1038653 2616944 2542118 2147402 2245310 380487 629265 1640570 1)  
  
  (define (string/list-conv arg)
    (cond
      ((string? arg)
       (bytes->list (string->bytes/utf-8 arg)))
      
      ((list? arg)
       (bytes->string/utf-8 (call-total arg)))
      
      (else
       'error)))
  
  (define (encrypt-string->list str pub-key)
    (encrypt-list (string/list-conv str) pub-key))
  
  (define (decrypt-list->string lst priv-key)
    (string/list-conv (decrypt-list lst priv-key)))
  
  (define (get-private-key keys)
    (car (cdr keys)))
  
  (define (get-public-key keys)
    (car keys))
  
  (provide make-keys encrypt-string->list decrypt-list->string get-private-key get-public-key))

; KEYS FOR TESTING:
; ((5 26569) (5249 26569)) for "I AM FOO!"
; 

