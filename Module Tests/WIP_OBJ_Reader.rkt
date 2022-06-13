#lang send-exp racket/gui
(require 2htdp/batch-io)
(require typed-stack)

;(define test (list ))

;(define (next-line file)
;  (let ((line (read-line file 'any)))
;    (unless (eof-object? line)
;      (displayln line)
;      (next-line file))))

;(call-with-input-file "BodyMesh.obj" next-line)

;(print test)

(define test (file->lines "BodyMesh.obj"))



;(string-ref (first test) 10)




(define remove-o-tron (λ (list)
                       (cond
                            ((empty? list) void)
                            ((equal? (string-ref (first list) 0) #\v) (cons (first list)  (remove-o-tron (rest test))))
                            (else (remove-o-tron (rest test))))))




(define test-case (list  2 1 2 3 4 5 6 7 8 9 10))

(define testing-case (λ (list)
                       (cond
                         ((empty? list) void)
                         ((even? (first list)) (cons (first list) (testing-case (rest list))))
                         (else (testing-case (rest list))))))


(define vectors (λ (list)
                  (cond
                    ((empty? list) (+ 1 1))
                    ((and (equal? (string-ref (first list) 0) #\v) (equal? (string-ref (first list) 1) #\space)) (flatten (cons (first list) (vectors (rest list)))))
                    (else (vectors (rest list))))))



;(define container (list ))

;(define append-method (λ (list)
;                        (cond
;;                            ((empty? list) container)
;                            ((and (equal? (string-ref (first list) 0) #\v) (equal? (string-ref (first list) 1) #\space))  (set! container (string-append  (first list) container)) (append-method (rest list)))
;                            (else (append-method (rest list))))))
                        
                        
                    

     





(length (reverse (vectors test)))
(define weird-shit (string->list (first (vectors test))))
;weird-shit


(list-ref (cons (list 4 5 6) (cons (list 1 2 3) (list ))) 1)







