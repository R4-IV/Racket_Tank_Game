#lang racket
(require racket/gui/base)

;----------------------Leveling testing---------------------------


;Experience Table distribution (to be curve)
(define ExpReq (hash
                1 300
                2 400
                3 600
                4 800
                5 1200
                6 1700
                7 2400
                8 3100
                9 4000
                10 5200
                11 6800
                12 9000
                13 12100
                14 15700
                15 20000
                16 24800
                17 31000
                18 37500
                19 48000
                20 63000
                21 81000
                22 100000
                23 125000
                24 162000
                25 197000
                25 245000
                26 300000
                27 359000
                28 440000
                29 558000
                30 700000))
                
                
                
                


(define LevelSystem%
  (class object%
    (init-field (level 1))
    (init-field (exp 0))
    (init-field (skill 1))
;level related functions
;increment level by 1
    (define/public (gain_level)
      (set! level (+ 1 level)))
;display/update current level 
    (define/public (display_level) level)
    
;exp related functions

;gain exp if you gain enough then gain a level and carry over the remaining into thr next exp requirement.
;Does not support gaining multiple levels at the moment
    (define/public (gain_exp n)
      (set! exp (+ n exp))
      (cond
        ((>= exp (hash-ref ExpReq (send PlayerSystem display_level)))
         (send PlayerSystem subtract_exp (hash-ref ExpReq (send PlayerSystem display_level)))
         (send PlayerSystem gain_level)
         (send PlayerSystem gain_skill))))
        
       
;Function used to take away exp used only when carrying over experience to the next level and subtracting previous exp.
      (define/public (subtract_exp n)
      (set! exp (- exp n)))
;display amount of experiance currently available     
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

(define base_exp_rate 1)                
(define growth_modifier (/ (send PlayerSystem display_level) 16))



;Basic Money system object (remember balance (add and remove))
(define BankSystem
  (class object%
    (init-field (balance 0))

    (define/public (gain_money n)
      (set! balance (+ n balance)))
    (define/public (lose_money n)
      (set! balance (- balance n)))
    (super-new)))

(define PlayerBank (new BankSystem))

