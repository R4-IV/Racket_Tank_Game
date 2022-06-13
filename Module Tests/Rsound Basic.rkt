#lang racket/gui
(require rsound)

(define music (rs-read  "Battle.wav"))   ; path to audio file (windows can only play .wav)

(rsound? music)

(play music)

(define a (list 1 2 3 4 5 6 7 8))

(sleep/yield 10)
(stop)



  