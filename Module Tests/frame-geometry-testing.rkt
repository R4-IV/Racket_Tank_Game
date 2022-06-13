#lang racket
(define frame (new frame%
                   [label "test"]
                   [width 700]
                   [height 500]))

(define canvas (new canvas%
                    [parent frame]))

(define panel (new horizontal-panel%
                   [parent frame]))

(define gauge (new gauge%
                   (label "")
                   [parent panel]
                   [min-width 300]
                   [min-height 50]
                   
                   
                   [range (hash-ref ExpReq (send PlayerSystem display_level))]))
(send gauge set-value (send PlayerSystem display_exp))

(define level_message(new message%
     [parent frame]
     [label (string-append "level " (number->string (send PlayerSystem display_level)))]))

(define available_skills (new message%
                              [parent frame]
                              [label (string-append "skills " (number->string (send PlayerSystem display_skill)))]))
(new button% [parent panel]
     [label "gain_exp"]
     [callback (λ (button event)
                 (send PlayerSystem gain_exp 50)
                 (send gauge set-range (hash-ref ExpReq (send PlayerSystem display_level)))
                 (send gauge set-value (send PlayerSystem display_exp)))])





(define second (new frame%
                    [width 700]
                    [height 500]
                    [label "second screen"]))

(define screen-shift (new button%
                          [parent frame]
                          [label "jump screen"]
                          [callback (λ (button event)
                                      (send second show #t)
                                      (send frame show #f))]))
(define screen-shift-pt2 (new button%
                          [parent second]
                          [label "go back"]
                          [callback (λ (button event)
                                      (send second show #f)
                                      (send frame show #t))]))
(define hide (new button%
                  [parent frame]
                  [label "hide the canvas"]
                  [callback (λ (button event)
                              (send canvas show #f))]))



(define start (send frame show #t))
start

