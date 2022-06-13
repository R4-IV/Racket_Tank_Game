#lang send-exp racket/gui
(require sgl/gl)
(require sgl/gl-vectors)


(define color 0)
(define color2 0)
(define polygon2 (glColor3f 1.0 1.0 1.0))
(define polygon3 (glColor3f 1.0 1.0 1.0))
(define polygon4 (glColor3f 1.0 1.0 1.0))
(define polygon5 (glColor3f 1.0 1.0 1.0))

(define multipress (make-hasheq))
(define (key-down! key) (hash-set! multipress key #t)) ;pressed
(define (key-up! key)   (hash-set! multipress key #f)) ;released

(define (key-down? key) (hash-ref  multipress key #f))

(define Clock (new timer%
                  [notify-callback (λ ()
                                     
                                       (when(key-down? #\space) (set! color 0) (send gl_enviroment on-paint))
                                       (unless (key-down? #\space)(set! color 1) (send gl_enviroment on-paint))
                                       (when(key-down? 'up) (set! color2 0) (send gl_enviroment on-paint))
                                       (unless (key-down? 'up) (set! color2 1) (send gl_enviroment on-paint)))]
                   [interval 30]))


(define resize (λ (w h)
                 (glViewport 0 0 w h)))



(define scene (λ ()
                (glClearColor 0.0 0.0 0.0 0.0)
                (glClear GL_COLOR_BUFFER_BIT)
                (glEnable GL_DEPTH_TEST)
                      
                      
                (glClear GL_DEPTH_BUFFER_BIT)

                (glShadeModel GL_SMOOTH)

                (glMatrixMode GL_PROJECTION)
                (glLoadIdentity)
                (glMatrixMode GL_MODELVIEW)
                (glLoadIdentity)

                (glBegin GL_POLYGON)
                (glColor3f color 1.0 1.0)
                
                (glVertex3f  0.1 -0.1 0.0)
                (glVertex3f  0.1  0.1 0.0)
                (glVertex3f -0.1  0.1  0.0)
                (glVertex3f -0.1 -0.1  0.0)
                (glEnd)

                 (glBegin GL_POLYGON)
                (glColor3f color2 1.0 1.0)
               
                (glVertex3f  0.4 -0.1 0.0)
                (glVertex3f  0.4  0.1 0.0)
                (glVertex3f  0.2  0.1  0.0)
                (glVertex3f  0.2 -0.1  0.0)
                (glEnd)

                   (glBegin GL_POLYGON)
                (glColor3f 1.0 1.0 1.0)
                
                (glVertex3f  0.7 -0.1 0.0)
                (glVertex3f  0.7  0.1 0.0)
                (glVertex3f  0.5  0.1  0.0)
                (glVertex3f  0.5 -0.1  0.0)
                (glEnd)

                   (glBegin GL_POLYGON)
                (glColor3f 1.0 1.0 1.0)
                
                (glVertex3f  -0.2 -0.1 0.0)
                (glVertex3f  -0.2  0.1 0.0)
                (glVertex3f  -0.4  0.1  0.0)
                (glVertex3f  -0.4 -0.1  0.0)
                (glEnd)

                (glBegin GL_POLYGON)
                (glColor3f 1.0 1.0 1.0)
                
                (glVertex3f  -0.5 -0.1 0.0)
                (glVertex3f  -0.5  0.1 0.0)
                (glVertex3f  -0.7  0.1  0.0)
                (glVertex3f  -0.7 -0.1  0.0)
                (glEnd)

                (glFlush)))


(define my-canvas%
  (class canvas%
    (inherit with-gl-context swap-gl-buffers)

    (define/override (on-paint)
      (with-gl-context (λ ()
                         (scene)
                         (swap-gl-buffers))))

    (define/override (on-size width height)
      (with-gl-context (λ ()
      (resize width height)
      (on-paint))))
    
    (define/override (on-char canvas)  
      (define key-pressed     (send canvas get-key-code))
      (define key-released    (send canvas get-key-release-code))
      
      (when (eq? key-released 'press) ;key is pressed
        (key-down! key-pressed))
      
      (when (eq? key-pressed 'release) ;key is not pressed   
        (key-up! key-released))
      
      (send this focus))

    (super-instantiate () (style (list 'gl)))))

(define test-window (new frame%
                         [label "multipress-test"]
                         [min-width 700]
                         [min-height 700]
                         ))

(define gl_enviroment (new my-canvas%
                           [parent test-window]))

(send test-window show #t)

                


