#lang send-exp racket/gui
(require sgl/gl)
(require sgl/gl-vectors)

(define (resize w h)
  (glViewport 0 0 w h))


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



(define camerax (λ (centx rad angle) (set! camx (* (+ centx rad) (cos angle )))))
(define cameray (λ (centy rad angle) (set! camy (* (+ centy rad) (cos angle)))))

 

(define camera (λ ( ) (gluLookAt camx camy 1 centerx centery 0 0 1 0)))



;(define on-key (λ (canvas event)
;                 (case (send event get-key-code)
;                 ((#\w) (set! L1 (+ L1 0.1)) (print " ")(print L1))
;                 ((#\s) (set! L1 (- L1 0.1)) (print " ")(print L1))
;                 ((down) (set! L2 (+ L2 0.1)) (print " ")(print L2))
;                 ((up) (set! L2 (- L2 0.1)) (print " ")(print L2))
;                 ((left) (set! L3 (+ L3 0.1)) (print " ")(print L3))
;                 ((right) (set! L3 (- L3 0.1)) (print " ")(print L3))
;                 ((#\e) (set! K1 (+ K1 0.1)) (print " ")(print K1))
;                 ((#\d) (set! K1 (- K1 0.1)) (print " ")(print K1))
;                 ((#\r) (set! K2 (+ K2 0.1)) (print " ")(print K2))
;                 ((#\f) (set! K2 (- K2 0.1)) (print " ")(print K2))
;                 ((#\t) (set! K3 (+ K3 0.1)) (print " ")(print K3))
;                 ((#\g) (set! K3 (- K3 0.1)) (print " ")(print K3))
;                 ((#\y) (set! M1 (+ M1 0.1)) (print " ")(print M1))
;                 ((#\h) (set! M1 (- M1 0.1)) (print " ")(print M1))
;                 ((#\u) (set! M2 (+ M2 0.1)) (print " ")(print M2))
;                 ((#\j) (set! M2 (- M2 0.1)) (print " ")(print M2))
;                 ((#\i) (set! M3 (+ M3 0.1)) (print " ")(print M3))
;                 ((#\k) (set! M3 (- M3 0.1)) (print " ")(print M3)))


;                 (send canvas refresh)
;                 (send canvas focus)))
                
                   
                   




(define draw-opengl (λ ()
                      (glEnable GL_DEPTH_TEST)
                      (glClearColor 0.0 0.0 0.0 0.0)
                      (glClear GL_COLOR_BUFFER_BIT)
                      (glClear GL_DEPTH_BUFFER_BIT)
                      
                      (glShadeModel GL_SMOOTH)


                     
                      (glMatrixMode GL_PROJECTION)
                      (glLoadIdentity)
                      (glMatrixMode GL_MODELVIEW)
                      (glLoadIdentity)
                      (glOrtho -3 3 -3 3 -3 3)
                      
                      (camera)
                      
    
;;Front side of the polygon
                      (glBegin GL_POLYGON)
                      (glColor3f -1.0 -1.0 1.0)
                      (glVertex3f -0.5 -0.5 -0.5)
                      (glColor3f 1.0 0.0 0.0)
                      (glVertex3f -0.5 0.5 -0.5)
                      (glColor3f 1.0 0.0 0.0)
                      (glVertex3f 0.5 0.5 -0.5)
                      (glColor3f 1.0 0.0 0.0)
                      (glVertex3f 0.5 -0.5 -0.5)   
                      (glEnd)
;;Back side of the polygon
                      (glBegin GL_POLYGON)
                      (glColor3f 0.5 0.0 1.0)
                      (glVertex3f 0.5 -0.5 0.5)                      
                      (glVertex3f 0.5 0.5 0.5)                      
                      (glVertex3f -0.5 0.5 0.5)
                      (glVertex3f -0.5 -0.5 0.5)   
                      (glEnd)
;;Right side of the polygon
                      (glBegin GL_POLYGON)
                      (glColor3f 1.0 0.0 1.0)
                      (glVertex3f 0.5 -0.5 -0.5)                      
                      (glVertex3f 0.5 0.5 -0.5)                      
                      (glVertex3f 0.5 0.5 0.5)
                      (glVertex3f 0.5 -0.5 0.5)   
                      (glEnd)
;;Left side of the polygon
                      (glBegin GL_POLYGON)
                      (glColor3f 0.0 1.0 0.0)
                      (glVertex3f -0.5 -0.5 0.5)                      
                      (glVertex3f -0.5 0.5 0.5)                      
                      (glVertex3f -0.5 0.5 -0.5)
                      (glVertex3f -0.5 -0.5 -0.5)   
                      (glEnd)
;;Top side of the polygon
                      (glBegin GL_POLYGON)
                      (glColor3f 0.0 1.0 1.0)
                      (glVertex3f 0.5 0.5 0.5)                      
                      (glVertex3f 0.5 0.5 -0.5)                      
                      (glVertex3f -0.5 0.5 -0.5)
                      (glVertex3f -0.5 0.5 0.5)   
                      (glEnd)
;;Buttom side of the polygon
                      (glBegin GL_POLYGON)
                      (glColor3f 1.0 1.0 0.0)
                      (glVertex3f 0.5 -0.5 -0.5)                      
                      (glVertex3f 0.5 -0.5 0.5)                      
                      (glVertex3f -0.5 -0.5 0.5)
                      (glVertex3f -0.5 -0.5 -0.5)   
                      (glEnd)

                      (glFlush)))
                      


                      




(define my-canvas%
  (class canvas%
    (inherit with-gl-context swap-gl-buffers)
    
    (define/override (on-paint)
      (with-gl-context (λ ()
                         (draw-opengl)
                         (swap-gl-buffers))))

    (define/override (on-size width height)
      (with-gl-context (λ ()
                         (resize width height)
                         (on-paint))))
    ;(define/override (on-char event)
   ;   (on-key this event))
    
    (super-instantiate () (style (list 'gl)))))
    


(define window (new frame%
                    [label "test"]
                    [min-width 600]
                    [min-height 600]))
                    




(define gl-enviroment (new my-canvas%
                           [parent window]))

(send window show #t)

(define Clock (new timer%
                   
                  [notify-callback (λ ()
                                    (set! degreex (add1 degreex)) (set! degreey (add1 degreey)) (camerax centerx radius (radianx degreex)) (cameray centery radius (radiany degreey))
                                    (send gl-enviroment refresh))]
                  [interval 16]))






