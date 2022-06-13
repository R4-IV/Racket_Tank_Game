#lang racket/gui

(require rsound)

(host-api 'paMME)
(define frame-rate 44100) ;Gotta make sure I am at 44100K Hz in system too.
(define world (rs-read "ButtonSwitch.wav"))
;(define tone (make-tone 440 1 frame-rate))
;(play tone)
(play world)
(sleep 5)
(stop)
;(rs-write tone "test-tone")