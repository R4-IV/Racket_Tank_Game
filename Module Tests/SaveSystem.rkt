#lang send-exp racket/gui
(require 2htdp/batch-io)



(define level 60)
(define exp 300000)
(define skills 50)
(define Player-name "BOBBY")
(define save-slot "Save01.txt")

(define save-var (list (number->string level) (number->string exp) (number->string skills) Player-name))

(define list->string (λ (list)
                       (string-join list  " ")))




(define SaveSystem (λ (slot) (write-file slot (list->string save-var))))




(define LoadSave (λ (slot) (regexp-split #px" " (first (read-lines slot)))))

(define Load-Function (λ (slot)
  (set! level (string->number (first (LoadSave slot))))
  (set! exp (string->number (second (LoadSave slot))))
  (set! skills (string->number (third (LoadSave slot))))
  (set! Player-name (fourth (LoadSave slot)))))




;level
;exp
;skills
;Player-name
(SaveSystem save-slot)
;(Load-Function save-slot)
;level
;exp
;skills
;Player-name



  
  




