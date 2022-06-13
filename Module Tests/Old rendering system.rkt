#lang racket/gui

(define test%
  (class object%
       (super-new)
       (init-field (x 0))
       (init-field (x-limit 10))

       (define/public (increment-x)
         (set! x (+ 1 x)))
       (define/public (get-x) x)
       (define/public (get-x-limit) x-limit)))


(define (Checker l)
  (cond
       ((empty? l) (set! L T))
       ((equal? (send (first l) get-x) (send (first l) get-x-limit)) (Checker (rest l)) (print "t"))
       (else (set! T (append (list(first l)) T)) (Checker (rest l)))))
       
(define T (list ))
(define L (list))

(define Clock-san (new timer%
                       [notify-callback (λ ()
                                          (set! T '())
                                          (set! L (append (list (make-object test% 0)) L))

                                          (for/list ([i L])
                                            (send i  increment-x))

                                          (Checker L)

                                          
                                          
                                          (println (length L)))]
                                          
                       
                                          

                       [interval 1000]))



                                         
  
  