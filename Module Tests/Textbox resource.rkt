#lang send-exp racket/gui
(require 2htdp/image)
(require (planet williams/animated-canvas/animated-canvas))


(define InitialDialogSize (list 100 50))
(define Diag (rectangle (first InitialDialogSize) (second InitialDialogSize) "outline" "black"))

(define-struct QuestDatabase (Name Dialog Num))
;(define Q-1 (QuestDatabase "Introduction" () "1"
;Multipress hash table that enables mutiple simultanous keyboard inputs
(define multipress (make-hasheq))
(define (key-down! key) (hash-set! multipress key #t)) ;pressed
(define (key-up! key)   (hash-set! multipress key #f)) ;released
(define (key-down? key) (hash-ref  multipress key #f)) ;default hash is #f until changed

;Sprites

;down Idle
(define link-didle1 (make-object bitmap% "Link-sprites_01.gif" 'gif/alpha))
(define link-didle2 (make-object bitmap% "Link-sprites_02.png" 'png/alpha))
(define link-didle3 (make-object bitmap% "Link-sprites_03.gif" 'gif/alpha))
;left Idle
(define link-lidle1 (make-object bitmap% "Link-sprites_05.gif" 'gif/alpha))
(define link-lidle2 (make-object bitmap% "Link-sprites_06.gif" 'gif/alpha))
(define link-lidle3 (make-object bitmap% "Link-sprites_07.gif" 'gif/alpha))
;right Idle
(define link-ridle1 (make-object bitmap% "Link-sprites_10.gif" 'gif/alpha))
(define link-ridle2 (make-object bitmap% "Link-sprites_11.gif" 'gif/alpha))
(define link-ridle3 (make-object bitmap% "Link-sprites_12.gif" 'gif/alpha))
;up Idle
(define link-uidle1 (make-object bitmap% "Link-sprites_08.gif" 'gif/alpha))
;down movement
(define link-dmov1 (make-object bitmap% "Link-sprites_13.gif" 'gif/alpha))
(define link-dmov2 (make-object bitmap% "Link-sprites_14.gif" 'gif/alpha))
(define link-dmov3 (make-object bitmap% "Link-sprites_15.gif" 'gif/alpha))
(define link-dmov4 (make-object bitmap% "Link-sprites_16.gif" 'gif/alpha))
(define link-dmov5 (make-object bitmap% "Link-sprites_17.gif" 'gif/alpha))
(define link-dmov6 (make-object bitmap% "Link-sprites_18.gif" 'gif/alpha))
(define link-dmov7 (make-object bitmap% "Link-sprites_19.gif" 'gif/alpha))
(define link-dmov8 (make-object bitmap% "Link-sprites_20.gif" 'gif/alpha))
(define link-dmov9 (make-object bitmap% "Link-sprites_21.gif" 'gif/alpha))
(define link-dmov10 (make-object bitmap% "Link-sprites_22.gif" 'gif/alpha))
;left movement
(define link-lmov1 (make-object bitmap% "Link-sprites_23.gif" 'gif/alpha))
(define link-lmov2 (make-object bitmap% "Link-sprites_24.gif" 'gif/alpha))
(define link-lmov3 (make-object bitmap% "Link-sprites_25.gif" 'gif/alpha))
(define link-lmov4 (make-object bitmap% "Link-sprites_26.gif" 'gif/alpha))
(define link-lmov5 (make-object bitmap% "Link-sprites_27.gif" 'gif/alpha))
(define link-lmov6 (make-object bitmap% "Link-sprites_28.gif" 'gif/alpha))
(define link-lmov7 (make-object bitmap% "Link-sprites_29.gif" 'gif/alpha))
(define link-lmov8 (make-object bitmap% "Link-sprites_30.gif" 'gif/alpha))
(define link-lmov9 (make-object bitmap% "Link-sprites_31.gif" 'gif/alpha))
(define link-lmov10 (make-object bitmap% "Link-sprites_32.gif" 'gif/alpha))
;up movement
(define link-umov1 (make-object bitmap% "Link-sprites_33.gif" 'gif/alpha))
(define link-umov2 (make-object bitmap% "Link-sprites_34.gif" 'gif/alpha))
(define link-umov3 (make-object bitmap% "Link-sprites_35.gif" 'gif/alpha))
(define link-umov4 (make-object bitmap% "Link-sprites_36.gif" 'gif/alpha))
(define link-umov5 (make-object bitmap% "Link-sprites_37.gif" 'gif/alpha))
(define link-umov6 (make-object bitmap% "Link-sprites_38.gif" 'gif/alpha))
(define link-umov7 (make-object bitmap% "Link-sprites_39.gif" 'gif/alpha))
(define link-umov8 (make-object bitmap% "Link-sprites_40.gif" 'gif/alpha))
(define link-umov9 (make-object bitmap% "Link-sprites_41.gif" 'gif/alpha))
(define link-umov10 (make-object bitmap% "Link-sprites_42.gif" 'gif/alpha))
;right movement
(define link-rmov1 (make-object bitmap% "Link-sprites_43.gif" 'gif/alpha))
(define link-rmov2 (make-object bitmap% "Link-sprites_44.gif" 'gif/alpha))
(define link-rmov3 (make-object bitmap% "Link-sprites_45.gif" 'gif/alpha))
(define link-rmov4 (make-object bitmap% "Link-sprites_46.gif" 'gif/alpha))
(define link-rmov5 (make-object bitmap% "Link-sprites_47.gif" 'gif/alpha))
(define link-rmov6 (make-object bitmap% "Link-sprites_48.gif" 'gif/alpha))
(define link-rmov7 (make-object bitmap% "Link-sprites_49.gif" 'gif/alpha))
(define link-rmov8 (make-object bitmap% "Link-sprites_50.gif" 'gif/alpha))
(define link-rmov9 (make-object bitmap% "Link-sprites_51.gif" 'gif/alpha))
(define link-rmov10 (make-object bitmap% "Link-sprites_52.gif" 'gif/alpha))

(define field (make-object bitmap% "MapProtoFiltered.png" 'png))

(define (DrawChat x y textlist mode) (let ([y-pos 5])
                                          (cond
                                            ((equal? 1 mode)
                                             (for ([cur-string textlist]) (let ([cur-string-length (string-length cur-string)])
                                                                   (cond
                                                                     ((> cur-string-length y-pos)(set! y-pos cur-string-length)))))
                                             (send dc set-brush "white" 'solid)
                                             (send dc set-pen "black" 1 'solid)
                                             (send dc draw-rounded-rectangle x y (* y-pos 10) (* 21 (length textlist)))
                                             (let ([how-much-inside-list (length textlist)])
                                               (for ([cur-string-t (reverse textlist)]) (send dc draw-text cur-string-t (+ x 5) (+ (- y 13) (* 15 how-much-inside-list)) (set! how-much-inside-list (- how-much-inside-list 1)))))))))
                                                         
                     
                                                         



;forward view motion
(define dmov-state (list link-dmov1 link-dmov2 link-dmov3 link-dmov4 link-dmov5 link-dmov6 link-dmov7 link-dmov8 link-dmov9 link-dmov10))
;forward view idle 
(define didle-state (list link-didle1 link-didle2 link-didle3))
;left view motion
(define lmov-state (list link-lmov1 link-lmov2 link-lmov3 link-lmov4 link-lmov5 link-lmov6 link-lmov7 link-lmov8 link-lmov9 link-lmov10))
;left view idle
(define lidle-state (list link-lidle1 link-lidle2 link-lidle3))
;back view motion
(define umov-state (list link-umov1 link-umov2 link-umov3 link-umov4 link-umov5 link-umov6 link-umov7 link-umov8 link-umov9 link-umov10))
;back view idle
(define uidle-state (list link-uidle1))
;right view motion
(define rmov-state (list link-rmov1 link-rmov2 link-rmov3 link-rmov4 link-rmov5 link-rmov6 link-rmov7 link-rmov8 link-rmov9 link-rmov10))
;right view idle
(define ridle-state (list link-ridle1)); link-ridle2 link-ridle3))



(define increment (λ (x)
                    (cond
                      ((or (equal? x 1) (> x 1)) (set! x 1))
                      (else (+ 0.001 x)))))

(define decrement (λ (x)
                    (cond
                      ((or (equal? x 0) (> 0 x)) (set! x 0))
                      (else (- x 0.001)))))

(define walk-increment (λ (x)
                         (+ 8 x)))

(define walk-decrement (λ (x)
                         (-  x 8 )))
(define direction-facing "down")

;direction hash
(define hash-direction (hash "left" lidle-state
                             "right" ridle-state
                             "down" didle-state
                             "up" uidle-state))

(define sh 0)
(define sv 0)

(define posx 100)
(define posy 100)





(define Clock (new timer%
                  [notify-callback (λ ()
                                     
                                       (when(key-down? #\space) (set! sv (increment sv)) (send canvas scroll sh sv) (send canvas refresh-now)) 
                                       

                                       
                                                                  
                                         
                                       
                                       ;(when(key-down? 'up)(set! sv (decrement sv)) (send canvas scroll sh sv))
                                       ;(when(key-down? 'left) (set! sh (decrement sh)) (send canvas scroll sh sv))
                                       ;(when (key-down? 'right) (set! sh (increment sh)) (send canvas scroll sh sv))
                                     ;(unless (or (key-down? 'down) (key-down? 'up) (key-down? 'left) (key-down? 'right)) 
                                     (unless (or (key-down? 'down) (key-down? 'up) (key-down? 'left) (key-down? 'right))
                                       
                                       (for ((i (hash-ref hash-direction direction-facing)))
                                         (send dc clear)
                                         (send dc2 draw-bitmap field 0 0 ) 
                                         (send dc draw-bitmap i posx posy)
                                         (DrawChat posx (- posy 30) (list "Hey Robert you can probably do it!" "I Link have faith!") 1)
                                         
                                         (send canvas swap-bitmaps)
                                         (sleep/yield 0.01)))

                                     
                                     
                                     (when(key-down? 'down) (set! direction-facing "down") 
                                       
                                       (for ((i dmov-state))
                                       ;(send dc clear )
                                       (send dc2 draw-bitmap field 0 0 )
                                       (set! posy(walk-increment posy))
                                       (send dc draw-bitmap i posx posy)
                                       
                                       (send canvas swap-bitmaps)
                                       (sleep/yield 0.01)))
                                       

                                     (when(key-down? 'up) (set! direction-facing "up")
                                       
                                       (for ((i umov-state))
                                      ; (send dc clear )
                                       (send dc2 draw-bitmap field 0 0 )
                                       (set! posy(walk-decrement  posy))
                                       (send dc draw-bitmap i posx posy)
                                       
                                       (send canvas swap-bitmaps)
                                       (sleep/yield 0.01)))
                                       
                                     (when(key-down? 'right) (set! direction-facing "right")
                                       
                                       (for ((i rmov-state))
                                      ; (send dc clear )
                                       (send dc2 draw-bitmap field 0 0 )
                                       (set! posx(walk-increment posx))
                                       (send dc draw-bitmap i posx posy)
                                       
                                       (send canvas swap-bitmaps)
                                       (sleep/yield 0.01)))
                                       
                                     (when(key-down? 'left) (set! direction-facing "left")
                                       
                                       (for ((i lmov-state))
                                     ;  (send dc clear )
                                       (send dc2 draw-bitmap field 0 0 )
                                       (set! posx(walk-decrement posx))
                                       (send dc draw-bitmap i posx posy)
                                       
                                       (send canvas swap-bitmaps)
                                       (sleep/yield 0.01))))]
                                       
                                     
                  
                  [interval 30]))






(define my-canvas%
  (class animated-canvas%
    (inherit get-dc swap-bitmaps)

   
                                                                      
    (define/override (on-char canvas)  
      (define key-pressed     (send canvas get-key-code)) ;get key pressed
      (define key-released    (send canvas get-key-release-code)) ;get key released 
      
      (when (eq? key-released 'press) ;key is pressed
        (key-down! key-pressed))
      
      (when (eq? key-pressed 'release) ;key is not pressed   
        (key-up! key-released))
      ;fullscreen function ready to be translated into a on mouse event 
      (when (eq? key-released #\f) (cond
                                        ((equal? #t (send Game-window is-fullscreened?)) (send Game-window fullscreen #f))
                                        ((equal? #f (send Game-window is-fullscreened?)) (send Game-window fullscreen #t))))
      
      (send this focus))

    (super-instantiate () (style (list 'hscroll 'vscroll )))))






(define Game-window (new frame%
                         [label "Game window"]
                         [min-width 500]
                         [min-height 500]))











                                                                            

(define canvas (new my-canvas%
                    [parent Game-window]))

(define dc (send canvas get-dc))
(define dc2 (send canvas get-dc))

                    
                  

(send Game-window show #t)
(send canvas init-auto-scrollbars 5000 5000 sh sv)


                    




                   
(send canvas get-virtual-size)
(send canvas init-auto-scrollbars 5000 5000 sh sv)



                   
                        
                          
