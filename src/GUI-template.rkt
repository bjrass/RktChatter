; This module contains the GUI, and hides all (most?) interactions
; with the actual components.

(module ChatterGui racket
  (require racket/gui/base)
  (require (file "Common.rkt"))
  
  (define ChatterGuiListener<%> (interface () gui-connect gui-choose-client gui-set-udp gui-send))
  
  (define ChatterGui%
    (class object%
      (super-new)
      (init frame)
      
      (define m-listener TRUE-NULL)
      
      (define (udp-checker t e)
        (unless (TRUE-NULL? m-listener)
          (send m-listener gui-set-udp (send t get-value))))
      
      (define (send-message t e)
        (unless (or (TRUE-NULL? m-listener) (not (eq? (send e get-event-type) 'text-field-enter)))
          (send m-listener gui-send (send t get-value))
          (send t set-value "")))
      
      (define (connect-pressed b e)
        (unless (TRUE-NULL? m-listener)
          (send m-listener gui-connect (send host-field get-value) (send port-field get-value))))
      
      (define (client-list-event l e)
        (unless (or (TRUE-NULL? m-listener))
          (let ((idx (send l get-selection)))
            (unless (eq? idx #f)
              (send m-listener gui-choose-client idx)))))
      
      (define window-pane (new vertical-pane% [parent frame]))
      (define main-pane (new horizontal-pane% [parent window-pane]))
      
      ; -------- LEFT VPANE ----------
      (define left-vpane (new vertical-pane% [parent main-pane]))
      
      (define conversation-label (new message% [parent left-vpane] [label ""]))
      
      (define conversation-field (new text-field% [parent left-vpane] [label #f] [enabled #f] [style '(multiple)]))
      
      (define input-field (new text-field% [parent left-vpane] [label #f] [callback send-message]
                               [init-value "Type, you FOOL!"] [style '(single)]))
      ; ------------------------------
      
      ; -------- RIGHT VPANE ---------
      (define right-vpane (new vertical-pane% [parent main-pane]))
      (send right-vpane set-alignment 'left 'center)
      
      (new message% [parent right-vpane] [label "Chatters"])
      (define connection-list (new list-box% [choices '()] [parent right-vpane] [label #f] [callback client-list-event]))
      (define connect-pane (new horizontal-pane% [parent right-vpane] [stretchable-height #f]))
      (define host-field (new text-field% [parent connect-pane] [label "host"] [style '(single)] [init-value "localhost"]))
      (define port-field (new text-field% [parent connect-pane] [label "port"] [style '(single)] [init-value "9000"]))
      (define connect-button (new button% [parent right-vpane] [label "connect"]
                                  [callback connect-pressed]))
      (define udp-checkbox (new check-box% [parent right-vpane] [label "UDP LAN-broadcast"] [callback udp-checker]))
      ; ------------------------------
      
      ; ------------ CONSOLE ---------
      (define console-field (new text-field% [parent window-pane] [label #f] [enabled #f] [style '(multiple)]))
      (send console-field stretchable-height #f)
      (send console-field min-height 150)
      ; ------------------------------
      
      (define/public (set-console log)
        (send console-field set-value log))
      
      (define/public (set-conversation-label label)
        (send conversation-label set-label label))
      
      (define/public (append-console message)
        (send console-field set-value (string-append (send console-field get-value) message)))
      
      (define/public (set-conversation conversation)
        (send conversation-field set-value conversation))
      
      (define/public (set-udp-checked bool)
        (send udp-checkbox set-value bool))
      
      (define/public (get-udp-checked)
        (send udp-checkbox get-value))
      
      (define/public (set-listener! listener)
        (set! m-listener listener))
      
      (define/public (update-ui client)
        (let ((choices null))
          (define (iter i)
            (when (>= i 0)
              (set! choices (cons (send (send client get-client i) get-hostname) choices))
              (iter (- i 1))))
          (iter (- (send client get-client-count) 1))
          (send connection-list set choices)
          (send connection-list refresh)))))
  
  (provide ChatterGuiListener<%> ChatterGui%))
