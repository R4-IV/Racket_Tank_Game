#lang send-exp racket/gui
(require rsound)


(define Music-1 (rs-read  "1BadApplezBeat.wav"))

(define my-stream (make-pstream ))

(pstream-queue my-stream Music-1 10)

(define window%
  (class frame%
       (define/augment (on-close)
         (stop))
    (super-new)))


(define window-inst
  (new window%
       [label "SoundTest"]
       [width 500]
       [height 500]))
       

       
(define slider
  (new slider%
     [label "volume"]
     [min-value 0]
     [max-value 100]
     [init-value 50]
     [parent window-inst]
     [callback (lambda (b e)
                 (pstream-set-volume! my-stream (/ (send slider get-value) 100)))]))

(send window-inst show #t)
               
           

