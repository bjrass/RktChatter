(require (file "ArrayList.rkt"))
(define foo (new ArrayList%))

(define (fill num)
  (unless (< num 0)
    (send foo insert! 0 num)
    (fill (- num 1))))

(fill 100)

(display (send foo get-size))

(define (display-all)
  (define (iter i)
    (unless (< i 0)
      (display "Item: ")
      (display (send foo get-at i))
      (newline)
      (iter (- i 1))))
  (iter (- (send foo get-size) 1)))

(display-all)

(send foo remove-at 50)
(send foo remove-at 23)
(send foo remove-at 100)
(send foo remove-at 0)
(send foo remove-at 7)
(send foo remove-at 8)
(send foo remove-at 9)
;(send foo remove-at 10)
;(send foo remove-at 11)
;(send foo remove-at 12)
;(send foo remove-at 13)
;(send foo remove-at 14)

(display-all)