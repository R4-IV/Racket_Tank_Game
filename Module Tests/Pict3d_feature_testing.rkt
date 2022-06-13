#lang send-exp racket/gui
(require pict3d)
(require sgl/gl)
(require sgl/gl-vectors)

(define increment (λ (x)
                    (+ 1 x)))



(define camx 1)
(define camy 1)
(define centerx 0)
(define centery 0)

(define radius 
                      (sqrt (+ (sqr (- centerx camx)) (sqr (- centery camy)))))


(define degreex 0)
(define degreey 0)


(define radianx (λ (x)
                  (* (/ pi 180) x)))

(define radiany (λ (y)
                  (* (/ pi 180) y)))

(define scale 0.1)

(define camerax (λ (centx rad angle) (set! camx (* (+ centx rad) (cos angle )))))
(define cameray (λ (centy rad angle) (set! camy (* (+ centy rad) (cos angle)))))



(define viewpoint (λ (x y) (basis 'camera (point-at (pos x y 1) (pos centerx centery 0)))))
(define scene1 (combine (viewpoint camx camy)  (combine (cube origin scale) (light (pos 0 1 1) (emitted "yellow" 1)))))
(define scene2 (combine (viewpoint camx camy) (combine (cube origin 1/2)  (light (pos 0 1 1) (emitted "green" 1)))))
(define scene3 (combine (cube origin 1/2) (light (pos 0 1 1) (emitted "red" 2))))



(define resize (λ (w h)
                 (glViewport 0 0 w h)))

(define multipress (make-hasheq))
(define (key-down! key) (hash-set! multipress key #t)) ;pressed
(define (key-up! key)   (hash-set! multipress key #f)) ;released

(define (key-down? key) (hash-ref  multipress key #f)) ;default hash is #f until changed



(define frame (new frame%
                   [label "3dpict test"]
                   [min-width 600]
                   [min-height 600]))


(define my-canvas%
  (class pict3d-canvas%
    (inherit with-gl-context swap-gl-buffers)

    

    (define/override (on-size width height)
      (with-gl-context (λ ()
      (resize width height))))
    
      
    
    (define/override (on-char canvas)  
      (define key-pressed     (send canvas get-key-code))
      (define key-released    (send canvas get-key-release-code))
      
      (when (eq? key-released 'press) ;key is pressed
        (key-down! key-pressed))
      
      (when (eq? key-pressed 'release) ;key is not pressed   
        (key-up! key-released))
      
      (send this focus))
    (super-new)))

    

(define 3d-test (new my-canvas%
                    [parent frame]
                    [pict3d scene1]))


(define Clock (new timer%
                  [notify-callback (λ ()
                                     (when(key-down? #\space) (send 3d-test set-pict3d scene2))
                                     
                                     (unless (key-down? #\space)(send 3d-test set-pict3d scene1))
                                     (when(key-down? #\a) (and (set! camx (increment camx)) (set! camy (increment camy))) (and (camerax centerx radius (radianx degreex)) (cameray centery radius (radiany degreey))) (send 3d-test set-pict3d scene1) (print camx))
                                     (when(key-down? #\s) (set! scale (increment scale)) (send 3d-test refresh-now ) (print scale))
                                     )]
                                     
                                    
                  [interval 16]))

(define AutoR (new timer%
                   [notify-callback (λ ()
                                      (set! degreex (increment degreex)) (camerax centerx radius (radianx degreex)) (send 3d-test set-pict3d scene1)
                                      (set! degreey (increment degreey)) (cameray centery radius (radiany degreey)) (send 3d-test set-pict3d scene1)
                                      (glFlush))]
                   [interval 100]))
                                     





(send frame show #t)





