#lang racket
(require 2htdp/universe)
(require racket/gui/base)

;----------------------Leveling testing---------------------------


;Experiance Table distribution
(define ExpReq (hash
                1 3000
                2 4000
                3 6000
                4 7000
                5 12000
                6 17000
                7 24000))
                
                
                
                
                


(define LevelSystem%
  (class object%
    (init-field (level 1))
    (init-field (exp 0))
    (init-field (skill 0))
;level related functions
    (define/public (gain_level)
      (set! level (+ 1 level)))
    (define/public (display_level) level)
    
;exp related functions
    (define/public (gain_exp n)
      (set! exp (+ n exp))
      (cond
        ((>= exp (hash-ref ExpReq (send PlayerSystem display_level)))
         (send PlayerSystem subtract_exp (hash-ref ExpReq (send PlayerSystem display_level)))
         (send PlayerSystem gain_level)
         (send PlayerSystem gain_skill))
        
        (else (print exp))))

      (define/public (subtract_exp n)
      (set! exp (- exp n)))
    (define/public (display_exp) exp)
    
;skill point related functions
    (define/public (gain_skill)
      (set! skill (+ 1 skill)))
    (define/public (lose_skill)
      (cond
        ((> skill 0) (set! skill (- skill 1)))
        (else (print "insufficient skill points"))))
    (define/public (display_skill) skill)

;Initialising the super class (updating)
    (super-new)))

;New object Level System
(define PlayerSystem (new LevelSystem%))






;testing command updates 
;(send PlayerSystem gain_exp 500)
;(send PlayerSystem display_exp)
;(send PlayerSystem display_skill)
;(send PlayerSystem display_level)
;(send PlayerSystem lose_skill)
;(send PlayerSystem gain_exp 253)
;(send PlayerSystem display_exp)
;(send PlayerSystem display_skill)
;(send PlayerSystem display_level)

;----------------------Visual Test---------------------


(define Clock (new timer%
                   [notify-callback (λ ()
                      (send PlayerSystem gain_exp 2)
                 (send gauge set-range (hash-ref ExpReq (send PlayerSystem display_level)))
                 (send gauge set-value (send PlayerSystem display_exp))
                                      (send level_message set-label (string-append "level " (number->string (send PlayerSystem display_level))))
                                      (send available_skills set-label (string-append "skills " (number->string (send PlayerSystem display_skill)))))]
                    [interval 16]))
                    
                    



(define frame (new frame%
                   [label "test"]
                   [width 700]
                   [height 500]))

(define gauge (new gauge%
                   (label "Experience ")
                   [parent frame]
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
(new button% [parent frame]
     [label "gain_exp"]
     [callback (λ (button event)
                 (send PlayerSystem gain_exp 500)
                 (send gauge set-range (hash-ref ExpReq (send PlayerSystem display_level)))
                 (send gauge set-value (send PlayerSystem display_exp)))])

(new button%
     [parent frame]
     [label "Kill Updator"]
     [callback (λ (button event)
                 (send Clock stop))])





(define start (send frame show #t))
start

