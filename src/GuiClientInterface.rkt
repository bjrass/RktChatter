(module GuiClientInterface racket
  (require racket/gui/base)
  (require (file "Common.rkt"))
  (require (file "Debug.rkt"))
  (require (file "LocalClient.rkt"))
  (require (file "GUI-template.rkt"))
  
  ;; A local client that has a GUI attached to it, along with
  ;; other interaction features.
  (define GuiClientInterface%
    (class* LocalClient% (ChatterGuiListener<%>)
      (super-new)
      
      (inherit connect)
      (inherit broadcast)
      (inherit get-client-count)
      (inherit get-client)
      (inherit has-client?)
      (inherit drop)
      (inherit create-encryption!)
      (inherit get-codec)
      (inherit start-accepting)
      (inherit stop-accepting)
      
      ; #########################
      ; ===== CHAT COMMANDS =====
      ; #########################
      (define m-chat-commands (make-hash))
      (define (add-chat-command name proc)
        (hash-set! m-chat-commands name proc))
      (define (exec-chat-command name)
        (if (hash-has-key? m-chat-commands name)
            ((hash-ref m-chat-commands name))
            (Dbg:feedback "GuiClientInterface" "exec-chat-command" "unknown command " name)))
      
      (define (cmd-clear-log)
        (unless (TRUE-NULL? m-active-client)
          (send m-active-client set-log "")
          (send m-gui set-conversation "")))
      
      (define (cmd-clear-console)
        (send m-gui set-console ""))
      
      (define (cmd-connect)
        (unless (TRUE-NULL? m-active-client)
          (send m-active-client connect)))
      
      (define (cmd-disconnect)
        (unless (TRUE-NULL? m-active-client)
          (send m-active-client disconnect)))
      
      (define (cmd-dc)
        (send this disconnect-all))
      
      (define (cmd-drop)
        (unless (TRUE-NULL? m-active-client)
          (drop m-active-client)))
      
      (define (cmd-encrypt)
        (create-encryption!))
      
      (define (cmd-keys)
        (Dbg:display* "pub: " (send (get-codec) get-public-key) " priv: " (send (get-codec) get-private-key) "\n"))
      
      (define (cmd-hide)
        (stop-accepting))
      
      (define (cmd-accept)
        (start-accepting))
      
      (define (cmd-state)
        (unless (TRUE-NULL? m-active-client)
          (if (send m-active-client is-connected?)
              (Dbg:display "Client connected\n")
              (Dbg:display "Client disconnected\n"))))
      
      (define (cmd-help)
        (Dbg:display* "Commands: \n"
                      "/clear-log\n"
                      "  clears the conversation with the active client\n"
                      "/clear-console\n"
                      "  clears the console\n"
                      "/connect\n"
                      "  attempts to connect to the active client\n"
                      "/disconnect\n"
                      "  disconnects from the active client\n"
                      "/dc\n"
                      "  disconnects all connections\n"
                      "/drop\n"
                      "  disconnects and discards the active client\n"
                      "/encrypt\n"
                      "  creates new random keys for RSA encryption\n"
                      "/rsa-keys\n"
                      "  displays the keys used in RSA encryption\n"
                      "/hide\n"
                      "  stops accepting incoming connections\n"
                      "/accept\n"
                      "  starts accepting incoming connections\n"
                      "/state"
                      "  displays the state of the active client"))
      
      (add-chat-command "/clear-log" cmd-clear-log)
      (add-chat-command "/clear-console" cmd-clear-console)
      (add-chat-command "/connect" cmd-connect)
      (add-chat-command "/disconnect" cmd-disconnect)
      (add-chat-command "/dc" cmd-dc)
      (add-chat-command "/drop" cmd-drop)
      (add-chat-command "/encrypt" cmd-encrypt)
      (add-chat-command "/help" cmd-help)
      (add-chat-command "/rsa-keys" cmd-keys)
      (add-chat-command "/hide" cmd-hide)
      (add-chat-command "/accept" cmd-accept)
      (add-chat-command "/state" cmd-state)
      
      ; #########################
      ; === END CHAT COMMANDS ===
      ; #########################
      
      (define m-frame (new frame% [label "Chatter"] [width 800] [height 600]))
      (define m-gui (new ChatterGui% [frame m-frame]))
      (define m-active-client TRUE-NULL)
      
      (send m-frame show #t)
      (send m-gui set-listener! this)
      
      ;; Sets the interface to interact with 'client'
      (define/private (activate-client client)
        (set! m-active-client client)
        (if (TRUE-NULL? client)
            (send m-gui set-conversation "")
            (send m-gui set-conversation (send client get-log))))
      
      ;; Callbacks for the gui
      (define/public (gui-connect host port)
        (connect host (string->number port)))
      
      (define/public (gui-set-udp on)
        #f)
      
      (define/public (gui-send message)
        (cond
          ((and (> (string-length message) 0) (eq? (string-ref message 0) #\/))
           (exec-chat-command message))
          
          ((not (TRUE-NULL? m-active-client))
           (send m-active-client set-log (string-append (send m-active-client get-log) "You:  " message "\n"))
           (send m-gui set-conversation (send m-active-client get-log))
           (send m-active-client send-message message 0 #t))))
      
      (define/public (gui-choose-client num)
        (when (and (>= num 0) (< num (get-client-count)))
          (activate-client (get-client num))))
      
      (define/override (clients-changed)
        (unless (or (TRUE-NULL? m-active-client)
                    (has-client? m-active-client))
          (activate-client TRUE-NULL))
        
        (when (and (TRUE-NULL? m-active-client) (> (get-client-count) 0))
          (activate-client (get-client 0)))
        
        (send m-gui update-ui this))
      
      ; TODO There is a log in the gui and there is a log
      ; in the clients that goes to the conversation in the gui.
      ; Make this cleaner (or, don't this is just a test, do it all right)
      ; ^v---> 0 Thread safty
      (define/override (log-update client)
        (when (eq? client m-active-client)
          (send m-gui set-conversation (send client get-log))))
      
      ;; Makes this clients GUI receive debugging output
      (define/public (grab-debug-output)
        (Dbg:set-base-display
         (lambda (msg)
           (define tmp (open-output-string))
           (display msg tmp)
           (send m-gui append-console (get-output-string tmp)))))
      
      ))
  
  (provide GuiClientInterface%))
