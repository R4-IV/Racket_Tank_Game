#lang send-exp racket/gui


(define multipress (make-hasheq))
(define (key-down! key) (hash-set! multipress key #t)) ;pressed
(define (key-up! key)   (hash-set! multipress key #f)) ;released
(define (key-down? key) (hash-ref  multipress key #f)) ;default hash is #f until changed



(define increment (λ (x)
                    (cond
                      ((or (equal? x 1) (> x 1)) (set! x 1))
                      (else (+ 0.01 x)))))

(define decrement (λ (x)
                    (cond
                      ((or (equal? x 0) (> 0 x)) (set! x 0))
                      (else (- x 0.01)))))


(define sh 0)
(define sv 0)

(define Clock (new timer%
                  [notify-callback (λ ()
                                     
                                       (when(key-down? 'down) (set! sv (increment sv)) (send canvas scroll sh sv) (send canvas refresh-now))
                                       (when(key-down? 'up)(set! sv (decrement sv)) (send canvas scroll sh sv))
                                       (when(key-down? 'left) (set! sh (decrement sh)) (send canvas scroll sh sv))
                                       (when (key-down? 'right) (set! sh (increment sh)) (send canvas scroll sh sv))
                                       (when (key-down? #\r) (send canvas get-virtual-size)))]
                                     
                  [interval 300]))



(define my-canvas%
  (class canvas%
                                      
    (define/override (on-char canvas)  
      (define key-pressed     (send canvas get-key-code)) ;get key pressed
      (define key-released    (send canvas get-key-release-code)) ;get key released 
      
      (when (eq? key-released 'press) ;key is pressed
        (key-down! key-pressed))
      
      (when (eq? key-pressed 'release) ;key is not pressed   
        (key-up! key-released))
      
      (send this focus))

    (super-instantiate () (style (list 'hscroll 'vscroll 'no-autoclear)))))

(define Game-window (new frame%
                         [label "Game window"]
                         [min-width 700]
                         [min-height 700]))


(define Link-stuff (make-object bitmap% "Link sprites.png"))
(define Mario (make-object bitmap% "Mario.png"))
(define link-fvi (make-object bitmap% "Link_sprites_01.gif"))


(define canvas (new my-canvas%
                    [parent Game-window]
                    [paint-callback (λ (canvas bitmap-dc)
                                      (send dc draw-bitmap link-fvi 10 10))]))
(define dc (send canvas get-dc))



;style 2 
;(define pos 10)

;(define mycanvas%
;  (class canvas%
;    (super-new)
;    (inherit get-dc)
;    (define/override (on-paint)
;      (let ([my-dc (get-dc)])
;        (send my-dc draw-bitmap Link-stuff pos pos)))))

;(define test2 (new mycanvas%
;                   [parent Game-window]))

(send Game-window show #t)
;(sleep 5)
;(send dc draw-bitmap Mario 50 50)
;(send canvas refresh-now)
                   
(send canvas get-virtual-size)
(send canvas init-auto-scrollbars 5000 5000 sh sv)

                     
                        
                          

