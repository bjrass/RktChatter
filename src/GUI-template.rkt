(define frame (new frame% [label "Gui Template for RktChatter"]))

(define window-pane (new horizontal-pane% [parent frame]))

; -------- LEFT VPANE ----------
(define left-vpane (new vertical-pane% [parent window-pane]))

(define conversation-label (new message% [parent left-vpane] [label "Conversation with User1"]))

(define conversation-field (new text-field% [parent left-vpane] [label #f] [enabled #f]
                                [init-value "User1 says: Hey STUD!\nUser1 says: LOL j/k\nYou say: Hey.\nUser1 says: What's going on??\nUser1 says: ;)\n"] [style '(multiple)]))

(define input-field (new text-field% [parent left-vpane] [label #f]
                         [init-value "Your message goes here"] [style '(single)]))

(define send-button (new button% [parent left-vpane]
                         [label "send"] [stretchable-width #t]))

; -------- RIGHT VPANE ---------
(define right-vpane (new vertical-pane% [parent window-pane]))
(send right-vpane set-alignment 'left 'center)

(new message% [parent right-vpane] [label "Chatters"])
(new list-box% [parent right-vpane]
     [label #f]
     [choices '("User1" "User2" "mr. Foo" "cpt. Bar")])
(define connect-pane (new horizontal-pane% [parent right-vpane] [stretchable-height #f]))
(new text-field% [parent connect-pane] [label "host"] [style '(single)] [init-value "localhost"])
(new text-field% [parent connect-pane] [label "port"] [style '(single)] [init-value "9000"])
(new button% [parent right-vpane] [label "connect"])
(new check-box% [parent right-vpane] [label "UDP LAN-broadcast"])

; Show the frame by calling its show method
(send frame show #t)
