#lang racket/gui
(require 2htdp/image
         (only-in mrlib/image-core render-image)) ;; provides image render function that renders image% onto a canvas via dc

(require "Racket-Animated-Canvas.rkt") ;Gives double buffered rendering (fixes flickering) (modified for virtual size) 
(require "bitmap_resources.rkt");; Provides all images and sounds from path directory
(require rsound);; Sound engine enables async playing of multiple sounds 
(require 2htdp/batch-io);; Library that allows reading and writing data to files of chosen extension (text files)





;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;; A function that checks whether the system os is 'Windows if it is it provides host-api a required function to play sound on windows.
(define System-Check (λ ()
  (cond
    ((equal? (system-type 'os ) 'windows ) (host-api 'paMME)
                                           (send canvas show-scrollbars #f #f))))) ;;Windows has issues with hiding scrollbars this function should ensure that they are hidden.







;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;;Aligns image in a circle (the argument is an image% or bitmap%)
(define (circle-align-middle x)
    (define w (image-width x))
    (define h (image-height x))
    (define d  (max w h)) ;;picks the greater number so that all corners of the image are contained within the circle    
    (define dx (/ w 2))   
    (define dy (/ h 2)) 
    (define blank-circle  (circle d "solid" (color 255 255 255 0))) ;; creates a transparent blank circle 
    (place-image/align x (- d dx) (- d dy) "left" "top" blank-circle)) ;; Alignment between the circle and the target image






;;---------------------------------------------------------------------------------------------------------------------------------------------------------
;DataTypes for Storage
(define myProfileID 1) ;; Profile where the data is to (or) loaded from
(define Level (list 5 10000)) ;; Level/Experience 
(define current-new-name "") ;; Player name initialised below
(define OwnedSkills 5) ;;Number of currently owned/unused skill points 
(define SkillIDs (list "")) ;;A list containing all the players currently owned skills




;;Converting all data into strings as thats the only type of data supported for writing (Atleast to my knowledge)
(define (list->string list)
  (string-join list " "))

(define myProfileID2 (number->string myProfileID))
  
(define Level1 (number->string (first Level)))

(define Level2 (number->string (second Level)))

(define OwnedSkills2 (number->string OwnedSkills))

(define SkillIDs2 (list->string SkillIDs))


;Save System -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
(define SaveID 0) ;ID at the begining of each profile type txt.

(define SaveData (list myProfileID2 Level1 Level2 current-new-name OwnedSkills2 SkillIDs2  )) ;;Contents of SaveData will be written to a chosen profile.txt



(define RealProfiles (list )) ;Save-Check logs existing profiles here.

(define (SaveFile-Get x) (string-append (number->string x)"profile.txt")) ;appends the ID and format "profile.txt" into one entity for use as path and filename.

(define (Save-Check x) ;This is a scanner and a logger, it logs already existing saves in format and increments to avoid overwrite.
  (cond
    ((file-exists? (SaveFile-Get SaveID))(set! RealProfiles (append RealProfiles (list (SaveFile-Get SaveID)))) (println (SaveFile-Get SaveID))(println SaveID) (set! SaveID (add1 SaveID)) (Save-Check (SaveFile-Get SaveID)))
    (else (println RealProfiles))))

(define (Save-Overwrite x y) (cond
                               ((equal? 1 x)(write-file (SaveFile-Get y) (list->string SaveData))))) ;Writes data to chosen profile.txt


;Load System -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
;(define LoadSave (regexp-split #px" " (first (read-lines SaveFile-Get SaveID))))
(define (LoadSave2 x) (regexp-split #px" " (first (read-lines x))));; reads the file creating a list by finding all the " " /#space values.

;Skill Trees -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

;;Skill tree WIP (likely will need to be converted to work with Health% Shield%
(define Health 0)
(define Shield 0)
(define ReloadTimeout 0)
(define Damage 0)
(define repair 0)

(define (SkillAssighn num) (cond
                             ((equal? num 1) (set! Health 250))
                             ((equal? num 2) (set! Shield 150))
                             ((equal? num 3) (set! ReloadTimeout 136))
                             ((equal? num 4) (set! Damage 5))
                             ((equal? num 5) (set! repair 200))))

(define (Skillz x) (for ([i  (rest(rest(rest(rest(rest (LoadSave2 x))))))]) (SkillAssighn (string->number i))
                     ))
                                      


;LoadMoreStuff
(define ProActive0 (list 0 "No_Data"))
(define ProActive1 (list 0 "No_Data"))
(define ProActive2 (list 0 "No_Data"))
(define ProActive3 (list 0 "No_Data"))
(define ProActive4 (list 0 "No_Data"))
(define ProActive5 (list 0 "No_Data"))
(define ProActive6 (list 0 "No_Data"))
(define ProActive7 (list 0 "No_Data"))
(define ProActive8 (list 0 "No_Data"))

(define (LoadProfile x) (cond
                          ((file-exists? x) (set! myProfileID (first (LoadSave2 x))) (set! Level (string->number (second (LoadSave2 x)))) (set! Level (string->number (third (LoadSave2 x)))) (set! current-new-name (fourth (LoadSave2 x))) (set! current-new-name (fourth (LoadSave2 x))) (set! OwnedSkills  (string->number (fifth (LoadSave2 x)))) (Skillz x))
                          (else void)))


  









;;Sound--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

(define frame-rate 48000) ;;System needs to be at 44100K (WINDOwS FUNCTION)
(define RecX 346) ;;Value for the size/volume ratio for the settings bar (sound control)

;;Pstream function controls sound ques (currenly broken on mac and non 'pamne port windows)

;(define new-pstream (make-pstream))
;(define weapon-pstream (make-pstream))
;(pstream-play new-pstream Music-2)
;(pstream-set-volume! new-pstream (/ RecX 346)) ;;Pstream volume control function 



;;Plays main menu music
(play Music-1)

;;Plays engine sound with cooldown on a loop
(define EngineSoundControl 0)
(define EngineSound
  (cond
    ((equal? EngineSoundControl 0)
    ; (pstream-play weapon-pstream Movement-1)
     (set!  EngineSoundControl (add1  EngineSoundControl )))
    ((equal?  EngineSoundControl 500) (set!  EngineSoundControl 0))
    (else (set!  EngineSoundControl (add1  EngineSoundControl )))))
















;;Current screen for rendering 
(define current-screen "screen 0.0")

;;Hash table containing values of different screens and their bitmap/image counterparts (used for updating current-screen)


(define hash-screen0   (hash "screen 0.0" Main-title-0
                             "screen 0.1" Main-title-1
                             "screen 0.2" Main-title-2
                             "screen 0.3" Main-title-3
                             "screen 0.4" Main-title-4
                             "screen 3.0" Agho-0
                             "screen 3.1" Agho-1
                             "screen 4.0" NewGame-2
                             "screen 4.1" NewGame-1
                             "screen 5.0" LoadGame
                             "screen 6.0" screen-6.0
                             "screen 6.1" screen-6.1
                             "screen 6.2" screen-6.2
                             "screen 6.3" screen-6.3
                             "screen 6.4" screen-6.4
                             "screen 6.5" screen-6.5
                             "screen ND" screenNF
                             "screen 8.0" NotReady
                             "screen 8.1" ReadyGo
                             "screen L" Defeat
                             "screen 9.0" SkillTree1))






  

  





;;format x1 x2 y1 y2
;;vectors containg all rectangular map colllison

(define c1 (vector 505 770 405 535))
(define c2 (vector 770 924 405 1510))
(define c3 (vector 924 1395 1140 1512))
(define c4 (vector 1606 2231 1140 1512))
(define c5 (vector 2076 2232 877 1139))
(define c6 (vector 2076 2232 405 665))
(define c7 (vector 505 769 2095 2222))
(define c8 (vector 1080 1135 1900 2288))
(define c9 (vector 1341 1396 1703 2095))
(define c10 (vector 1396 1607 1965 2095))
(define c11 (vector 1761 1974 2157 2288))
(define c12 (vector 2912 3335 2936 3067))
(define c13 (vector 2650 2911 2482 2613))
(define c14 (vector 3280 3335 2157 2597))
(define c15 (vector 3124 3281 2157 2221))
(define c16 (vector 3123 3178 990 1703))
(define c17 (vector 2860 2914 1900 2288))
(define c18 (vector 2600 2860 2158 2289))
(define c19 (vector 2128 2337 1639 1766))
(define c20 (vector 2338 2390 1444 1766))
(define c21 (vector 2704 2759 1185 1511))
(define c22 (vector 2546 2704 1184 1315))
(define c23 (vector 2022 2075 857 1140))
(define c24 (vector 1657 2022 999 1140))
(define c25 (vector 1604 1657 858 1140))
(define c26 (vector 1341 1395 858 1140))
(define c27 (vector 978 1340 990 1140))
(define c28 (vector 924 978 405 1139))
(define c29 (vector 979 2023 405 536))
(define c30 (vector 2022 2077 405 663))
(define c31 (vector 2338 2549 534 664))
(define c32 (vector 2550 2601 534 988))
(define c33 (vector 0 505 0 3800))
(define c34 (vector 0 4000 0 405))
(define c35 (vector 3495 4000 0 3800))
(define c36 (vector 0 4000 3395 3800))


;;Spatial hash table splitting all the different vectors that need to be tested in each individual zone the tank currently sits in

(define spatial-collision (hash
                           1(list c1 c2 c33 c34)
                           2(list c28 c29 c26 c25 c27 c24 c23 c30)
                           3(list c34 c6 c5 c31 c32)
                           4(list c34 c35 c16)
                           5(list c33 c2 c3)
                           6(list c2 c3 c26 c25 c24 c27 c4 c9 c10 c8)
                           7(list c4 c5 c20 c24 c19 c32 c22 c21 c17)
                           8(list c16 c35)
                           9(list c33 c7 c8)
                           10(list c8 c9 c10 c11)
                           11(list c11 c17 c18 c13 c15)
                           12(list c15 c14 c35 c17 c18 c13 c12)
                           13(list c33 c36)
                           14(list c36)
                           15(list c36 c12)
                           16(list c12 c36 c35)))


                          
                         
;values for scrollbars 
(define scroll-horiz 0) ;;horizontal value accepts 0.0 -> 1.0
(define scroll-vert 0)  ;;vertical value accepts 0.0 ->1.0

;sizes for virtual canvas 
(define virtual-w 4000) ;;Sets x coordinate range for the canvas 
(define virtual-h 3800) ;;Sets y coordinate range for the canvas


;Variables controlling the countdown timer visible in screen mode 2 (GAME SCREEN)
(define minutes 3)
(define seconds 0)

;Variable controlling whether the minimap is visible or not (1 visible 0 not visible) see %mycanvas for key controls 
(define map-loading 1) ;;Mini-Map Loading Function

 

(define current-bullet standard-bullet) ;;Bullet Selection for the player (currently affects both the player and the enemy 

(define loading-var 0) ;;Value for pre-game load screen (init value for the red bar)




;----------------------------------------------------------------------------------------------------------
;Player rotation-val for the tank 
(define rotation-val 0)

;Enemy rotation-val for the tank 
(define enemy-rotation-val 0)


(define turret-rotation 0) ;;Rotation of the turret (stores angle of -180 -> 180
(define enemy-turret-rotation 0) ;;Rotation of the turret (stores angle of -180 -> 180


;Spawn point Player-Tank
(define ptx 550);; player tank x position 
(define pty 550);; player tank y position 

;Spawn point Enemy-Tank
(define etx 2850);; enemy tank x position
(define ety 750);; enemy tank y position

(define y-speed -8);; y speed for the tank (initial)
(define x-speed 0);; x speed for the tank (initial)

;Enemy
(define ey-speed -8)
(define ex-speed 8)



;;Player Tank images set up for rendering 
(define Tank  (scale/xy 0.2 0.2 Heavy-Tank))
(define Turret (scale/xy 0.2 0.2 Buster-Heavy))

;;Enemy Tank images set up for rendering 
(define Enemy-Tank (scale/xy 0.2 0.2 Heavy-Tank))
(define Enemy-Turret (scale/xy 0.2 0.2 Heavy-Battery))



;;Player Tank movement
;;Changes x y coordinates by adding current x and y speed splits 
(define (movement)
  (set! ptx (+ ptx x-speed))
  (set! pty (+ pty y-speed)))


;;Does the exact opposite of movement making it a great function for reversing 
(define (inverse-movement)
  (set! ptx (- ptx x-speed))
  (set! pty (- pty y-speed)))



;;Enemy Tank movement
;;Counterpart to player tank movement 
(define (enemy-movement)
  (set! etx (+ etx ex-speed))
  (set! ety (+ ety ey-speed)))

(define (inverse-enemy-movement)
  (set! etx (- etx ex-speed))
  (set! ety (- ety ey-speed)))




; Collision of individual points player (x,y)
(define cpg 0)
(define cpb 0)
(define cpw 0)
(define cpp 0)

; Collision of individual points enemy (x,y)
(define epg 0)
(define epb 0)
(define epw 0)
(define epp 0)
  
;------------------------------------------------------------------------------------------------------------------






;;Collision----------------------------------------------------------------------------------------------------------------------------------------------------------------------

;; A variable that stores the collision list retrieved from the spatial hash table 
(define collision-list '())



;; A function that takes all the individual player points and checks if they are colliding if they are it performs a certain action
(define (collision-behaviour-player)
  (cond
    ((and (equal? cpg 1) (equal? cpw 1)) (cond
                                           ((>= 90 rotation-val ) (forward-movement (ptcx Tank) (ptcy Tank)  (+ rotation-val 270)) (movement) (set! cpg 0) (set! cpw 0))
                                           (else   (forward-movement (ptcx Tank) (ptcy Tank)  (-(+ rotation-val 270) 360)) (movement) (set! cpg 0) (set! cpw 0))))
;; Checks if there are 2 diagnal points colliding and if they are movement is produced perpendicular to the angle of the direction facing     
    ((and (equal? cpb 1) (equal? cpp 1)) (cond
                                           ((>= 270 rotation-val) (forward-movement (ptcx Tank) (ptcy Tank)  (+ rotation-val 90)) (movement)  (set! cpb 0) (set! cpp 0))
                                           (else (forward-movement (ptcx Tank) (ptcy Tank)  (- (+ rotation-val 90) 360)) (movement)  (set! cpb 0) (set! cpp 0))))
    
    ((or (equal? cpg 1) (equal? cpb 1)) (inverse-movement) (set! cpg 0) (set! cpb 0)) ;;front points or back points colliding will result in a movement function opposite of the current direction moving
    ((or (equal? cpw 1) (equal? cpp 1)) (movement) (set! cpw 0) (set! cpp 0)) 
    (else void))) ;;if there is no collision do nothing 


;; Counterpart to player behaviour for collision (eventually the above fuction can be converted to take indivdual points)
(define (collision-behaviour-enemy)
  (cond
    ((and (equal? epg 1) (equal? epw 1)) (cond
                                           ((>= 90 rotation-val ) (forward-movement (etcx Enemy-Tank) (etcy Enemy-Tank)  (+ enemy-rotation-val 270)) (enemy-movement) (set! epg 0) (set! epw 0))
                                           (else   (forward-movement (etcx Enemy-Tank) (etcy Enemy-Tank)  (-(+ enemy-rotation-val 270) 360)) (enemy-movement) (set! epg 0) (set! epw 0))))
    
    ((and (equal? epb 1) (equal? epp 1)) (cond
                                           ((>= 270 enemy-rotation-val) (forward-movement (etcx Enemy-Tank) (etcy Enemy-Tank)  (+ enemy-rotation-val 90)) (enemy-movement)  (set! epb 0) (set! epp 0))
                                           (else (forward-movement (etcx Enemy-Tank) (etcy Enemy-Tank)  (- (+ enemy-rotation-val 90) 360)) (movement)  (set! epb 0) (set! epp 0))))
    
    ((or (equal? epg 1) (equal? epb 1)) (inverse-enemy-movement) (set! epg 0) (set! epb 0))
    ((or (equal? epw 1) (equal? epp 1)) (enemy-movement) (set! epw 0) (set! epp 0))
    (else void)))
                                    
  


;; A function that finds the centerpoint of an object (currently only tanks) and sets the collision list to hold the contents of the spatial hash at that loacation.
(define (find-object cx cy)
  (cond
    ((and (and (> cx 0) (>= 1000 cx)) (and (> cy 0) (>= 950 cy))) (set! collision-list (hash-ref spatial-collision 1))) 
     ((and (and (> cx 1000) (>= 2000 cx)) (and (> cy 0) (>= 950 cy))) (set! collision-list(hash-ref spatial-collision 2))) 
      ((and (and (>  cx 2000) (>= 3000 cx)) (and (> cy 0) (>= 950 cy))) (set! collision-list(hash-ref spatial-collision 3))) 
       ((and (and (>  cx 3000) (>= 4000 cx)) (and (> cy 0) (>= 950 cy))) (set! collision-list(hash-ref spatial-collision 4))) 
        ((and (and (> cx 0) (>= 1000 cx)) (and (> cy 950) (>= 1900 cy))) (set! collision-list(hash-ref spatial-collision 5))) 
         ((and (and (> cx 1000) (>= 2000 cx)) (and (> cy 950) (>= 1900 cy))) (set! collision-list(hash-ref spatial-collision 6)))
          ((and (and (> cx 2000) (>= 3000 cx)) (and (> cy 950) (>= 1900 cy))) (set! collision-list(hash-ref spatial-collision 7)))
           ((and (and (> cx 3000) (>= 4000 cx)) (and (> cy 950) (>= 1900 cy))) (set! collision-list(hash-ref spatial-collision 8)))
            ((and (and (> cx 0) (>= 1000 cx)) (and (> cy 1900) (>= 2850  cy))) (set! collision-list(hash-ref spatial-collision 9)))
             ((and (and (> cx 1000) (>= 2000 cx)) (and (> cy 1900) (>= 2850 cy))) (set! collision-list(hash-ref spatial-collision 10)))
              ((and (and (> cx 2000) (>= 3000 cx)) (and (> cy 1900) (>= 2850 cy))) (set! collision-list(hash-ref spatial-collision 11)))
               ((and (and (> cx 3000) (>= 4000 cx)) (and (> cy 1900) (>= 2850 cy))) (set! collision-list(hash-ref spatial-collision 12)))
                ((and (and (> cx 0) (>= 1000 cx)) (and (> cy 2850) (>= 3800 cy))) (set! collision-list(hash-ref spatial-collision 13)))
                 ((and (and (> cx 1000) (>= 2000 cx)) (and (> cy 2850) (>= 3800 cy))) (set! collision-list(hash-ref spatial-collision 14)))
                  ((and (and (> cx 2000) (>= 3000 cx)) (and (> cy 2850) (>= 3800 cy))) (set! collision-list(hash-ref spatial-collision 15)))
                   ((and (and (> cx 3000) (>= 4000 cx)) (and (> cy 2850) (>= 3800 cy))) (set! collision-list(hash-ref spatial-collision 16)))))






;;A function that checks collision vectors from the collision list against each of the 4 x and 4 y values and determines whether their collision points should evaluate to 0 or 1
(define (collision-checker x list)
  (let* ([x1 (send x get-xcoord1)]
         [x2 (send x get-xcoord2)];;Let function to cut down on re writting all this forever 
         [x3 (send x get-xcoord3)]
         [x4 (send x get-xcoord4)]
         [y1 (send x get-ycoord1)]
         [y2 (send x get-ycoord2)]
         [y3 (send x get-ycoord3)]
         [y4 (send x get-ycoord4)])
  (cond
    ((empty? list) void)
    ((and (and (< (vector-ref (first list) 0) x1) (> (vector-ref (first list) 1) x1)) (and (<(vector-ref (first list) 2) y1) (> (vector-ref (first list) 3) y1))) (set! cpb 1) (collision-checker x (rest list)))                                                                                                                                                                                                                                                                                                                                       
    ((and (and (< (vector-ref (first list) 0) x2) (> (vector-ref (first list) 1) x2)) (and (<(vector-ref (first list) 2) y2) (> (vector-ref (first list) 3) y2))) (set! cpg 1) (collision-checker x (rest list)))                                                                                                                                                                         
    ((and (and (< (vector-ref (first list) 0) x3) (> (vector-ref (first list) 1) x3)) (and (<(vector-ref (first list) 2) y3) (> (vector-ref (first list) 3) y3))) (set! cpw 1) (collision-checker x (rest list)))      
    ((and (and (< (vector-ref (first list) 0) x4) (> (vector-ref (first list) 1) x4)) (and (<(vector-ref (first list) 2) y4) (> (vector-ref (first list) 3) y4))) (set! cpp 1) (collision-checker x (rest list)))
    (else (collision-checker x (rest list)))))) ;;no collision on the first object then recursion to check the next 



;;Counterpart for the enemy (likely redundant as it takes the same arguments as above)
(define (collision-checker-e x list)
  (let* ([x1 (send x get-xcoord1)]
         [x2 (send x get-xcoord2)]
         [x3 (send x get-xcoord3)]
         [x4 (send x get-xcoord4)]
         [y1 (send x get-ycoord1)]
         [y2 (send x get-ycoord2)]
         [y3 (send x get-ycoord3)]
         [y4 (send x get-ycoord4)])
  (cond
    ((empty? list) void)
    ((and (and (< (vector-ref (first list) 0) x1) (> (vector-ref (first list) 1) x1)) (and (<(vector-ref (first list) 2) y1) (> (vector-ref (first list) 3) y1))) (set! epb 1) (collision-checker x (rest list)))                                                                                                                                                                                                                                                                                                                                       
    ((and (and (< (vector-ref (first list) 0) x2) (> (vector-ref (first list) 1) x2)) (and (<(vector-ref (first list) 2) y2) (> (vector-ref (first list) 3) y2))) (set! epg 1) (collision-checker x (rest list)))                                                                                                                                                                         
    ((and (and (< (vector-ref (first list) 0) x3) (> (vector-ref (first list) 1) x3)) (and (<(vector-ref (first list) 2) y3) (> (vector-ref (first list) 3) y3))) (set! epw 1) (collision-checker x (rest list)))      
    ((and (and (< (vector-ref (first list) 0) x4) (> (vector-ref (first list) 1) x4)) (and (<(vector-ref (first list) 2) y4) (> (vector-ref (first list) 3) y4))) (set! epp 1) (collision-checker x (rest list)))
    (else (collision-checker x (rest list))))))
    
;;Objects--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------                                                                       

;;Reload object for the player
;;in charge of reload speed and the size of the reload bar 
(define reload%
  (class object%
    (super-new)
    (init-field (recharge 196))
    (init-field (rspeed 3))

    (define/public (reset-recharge)
      (set! recharge 0))

    (define/public (recharge-reload)
      (set! recharge (+ recharge rspeed)))

    (define/public (increase-rspeed x)
      (set! rspeed (* rspeed x)))

    (define/public (get-recharge) recharge)
    (define/public (get-rspeed) rspeed)))


;;Health behaves same as shield but for health instead 
(define Health%
  (class object%
    (super-new)
    (init-field (max-health 200))
    (init-field (current-health 200))

    (define/public (gain-health x)
      (cond
        ((> (+ x current-health) max-health) (set! current-health max-health))
        (else (set! current-health (+ x current-health)))))

    (define/public (lose-health x)
      (cond
        ((< (- current-health x) 0 ) (set! current-health 0))
        (else (set! current-health (- current-health x)))))

    (define/public (get-health) current-health)
    (define/public (get-max-health) max-health)))


;;Shield object (keeps track of all shield related shenanigans)
(define Shield%
  (class object%
    (super-new)
    (init-field (max-shield 200))
    (init-field (current-shield 200))

    (define/public (gain-shield x)
      (cond
        ((> (+ x current-shield) max-shield) (set! current-shield max-shield))
        (else (set! current-shield (+ x current-shield)))))

    (define/public (lose-shield x)
      (cond
        ((< (- current-shield x) 0 ) (set! current-shield 0))
        (else (set! current-shield (- current-shield x)))))

    (define/public (get-shield) current-shield)
    (define/public (get-max-shield) max-shield)))


;;Projectile object for bullet firing
;;In charge of bullet spawn and flight pathing with a currently unused field that can eventually be used 
(define projectile%
  (class object%
    (super-new)
    (init-field (x-origin 0))
    (init-field (y-origin 0))
    (init-field (ux 0))
    (init-field (uy 0))
    (init-field (rotation 0))
    (init-field (radius 0))
    (init-field (radius-range 400))
    (init-field (bullet-type current-bullet))
  

    (define/public (get-x-origin ) x-origin)
    (define/public (get-y-origin ) y-origin)
    (define/public (get-ux) ux)
    (define/public (get-uy) uy)
    (define/public (get-rotation ) rotation)
    (define/public (get-radius ) radius)
    (define/public (get-radius-range ) radius-range)
    (define/public (get-bullet-type) bullet-type)

    (define/public (set-bullet-type x)
      (set! bullet-type x))    
    (define/public (x-coordinate )
        (set! ux (+ x-origin (* radius (- 0(sin (* rotation (/ pi 180))))))))    
    (define/public (increment-radius)
      (set! radius (+ 20 radius)))      
    (define/public (y-coordinate )
      (set! uy (+ y-origin (* radius (- 0(cos (* rotation (/ pi 180))))))))))

;;Collision object keeps track of object coordinates
;;Currently exclusively used for tank points 
(define collision%
  (class object%
    (super-new)
    (init-field (xcoord1 0))
    (init-field (xcoord2 0))
    (init-field (xcoord3 0))
    (init-field (xcoord4 0))
    (init-field (ycoord1 0))    
    (init-field (ycoord2 0))
    (init-field (ycoord3 0))
    (init-field (ycoord4 0))
    

    (define/public (get-xcoord1) xcoord1)
    (define/public (get-xcoord2) xcoord2)
    (define/public (get-xcoord3) xcoord3)
    (define/public (get-xcoord4) xcoord4)
    (define/public (get-ycoord1) ycoord1)
    (define/public (get-ycoord2) ycoord2)
    (define/public (get-ycoord3) ycoord3)
    (define/public (get-ycoord4) ycoord4)


    (define/public (update-xcoord1 x)
      (set! xcoord1 x))
    (define/public (update-xcoord2 x)
      (set! xcoord2 x))    
    (define/public (update-xcoord3 x)
      (set! xcoord3 x))
    (define/public (update-xcoord4 x)
      (set! xcoord4 x))

     (define/public (update-ycoord1 x)
      (set! ycoord1 x))
    (define/public (update-ycoord2 x)
      (set! ycoord2 x))
 (define/public (update-ycoord3 x)
      (set! ycoord3 x))
 (define/public (update-ycoord4 x)
      (set! ycoord4 x))))




    
     


(define player-health (make-object Health% 200 200));;player health instance
(define enemy-health (make-object Health% 200 200));;enemy health instance
(define player-shield (make-object Shield% 200 200));;player shield instance 
(define enemy-shield (make-object Shield% 200 200));;enemy shield instance 


(define player-loading (make-object reload% 196 10));; Player reload instance 

(define player-c (make-object collision% ptx (+ ptx (image-width (circle-align-middle Tank))) 0 0 pty (+ pty (image-height (circle-align-middle Tank))) 0 0)) ;; player coordinates 
(define enemy-c (make-object collision% etx (+ etx (image-width (circle-align-middle Enemy-Tank))) 0 0 ety (+ ety (image-height (circle-align-middle Enemy-Tank))) 0 0)) ;; enemy coordinates 


;Multipress hash table that enables mutiple simultanous keyboard inputs
;---------------------------------------------------------------------------------------
(define multipress (make-hasheq))
(define (key-down! key) (hash-set! multipress key #t)) ;pressed
(define (key-up! key)   (hash-set! multipress key #f)) ;released
(define (key-down? key) (hash-ref  multipress key #f)) ;default hash is #f until changed this is done so no run time error returns 
;---------------------------------------------------------------------------------------


;trig functions
;;distance between points 
(define (distance-points x1 x2 y1 y2)
  (sqrt (+ (sqr (- x2 x1)) (sqr (- y2 y1)))))


;;law of cosines
(define (cosC a b c)
  (acos (/ (- (sqr c) (sqr b) (sqr a)) (* -2 a b))))


      


;; A list containing all current Projectile% instances for rendering                         
(define Projectile-List '())


;; A function that can be called to add a projectile% to the projectile list at a given angle and at 70r offset from the tanks central point
(define Bullet-Storm (λ ()
  (set! Projectile-List (append (list (make-object projectile% (ptcx Tank) (ptcy Tank) (bulletx Tank turret-rotation 70) (bullety Tank turret-rotation 70)  turret-rotation 70 400)) Projectile-List))))

;;Counterpart to the players append projectile function
(define Bullet-Storm-E (λ ()
  (set! Projectile-List (append (list (make-object projectile% (etcx Enemy-Tank) (etcy Enemy-Tank) (bulletx Enemy-Tank enemy-turret-rotation 70) (bullety Enemy-Tank enemy-turret-rotation 70)  enemy-turret-rotation 70 400)) Projectile-List))))
                                 



;;Renders bullets based on projectile object
(define (Bullet-Renderer l)
  (cond
    ((empty? l) void)
    (else 
          (render-image (rotate (send (first l) get-rotation) (circle-align-middle (send (first l) get-bullet-type)))
                        dc (- (send (first l) get-ux) 34) (-(send (first l) get-uy)34)) (cond
                                                                                          
                                                                                           ((and (and (> rotation-val 0) (>= 90 rotation-val))  (and (< (send player-c get-xcoord2) (- (send (first l) get-ux) 34)) (> (send player-c get-xcoord1) (- (send (first l) get-ux) 34))) (and (< (send player-c get-ycoord1) (-(send (first l) get-uy)34)) (> (send player-c get-ycoord3) (-(send (first l) get-uy)34)))) (cond
                                                                                                                                                                                                                                                                                                                                                                        ((equal? (send player-health get-health) 0 ) (set! screen-mode 11) (set! current-screen "screen L"))
                                                                                                                                                                                                                                                                                                                                                                        ((equal? (send player-shield get-shield) 0) (send player-health lose-health 10) (Bullet-Renderer (rest l)))
                                                                                                                                                                                                                                                                                                                                                                        (else (send player-shield lose-shield 10) (Bullet-Renderer (rest l)))))
                                                                                            ((and (and (> rotation-val 90) (>= 180 rotation-val)) (and (< (send player-c get-xcoord1) (- (send (first l) get-ux) 34)) (> (send player-c get-xcoord3) (- (send (first l) get-ux) 34))) (and (< (send player-c get-ycoord1) (-(send (first l) get-uy)34)) (> (send player-c get-ycoord2) (-(send (first l) get-uy)34)))) (cond
                                                                                                                                                                                                                                                                                                                                                                        ((equal? (send player-health get-health) 0 ) (set! screen-mode 11) (set! current-screen "screen L"))
                                                                                                                                                                                                                                                                                                                                                                        ((equal? (send player-shield get-shield) 0) (send player-health lose-health 10) (Bullet-Renderer (rest l)))
                                                                                                                                                                                                                                                                                                                                                                        (else (send player-shield lose-shield 10) (Bullet-Renderer (rest l)))))
                                                                                            ((and (and (> rotation-val 180) (>= 270 rotation-val))  (and (< (send player-c get-xcoord1) (- (send (first l) get-ux) 34)) (> (send player-c get-xcoord2) (- (send (first l) get-ux) 34))) (and (< (send player-c get-ycoord3) (-(send (first l) get-uy)34)) (> (send player-c get-ycoord1) (-(send (first l) get-uy)34)))) (cond
                                                                                                                                                                                                                                                                                                                                                                        ((equal? (send player-health get-health) 0 ) (set! screen-mode 11) (set! current-screen "screen L"))
                                                                                                                                                                                                                                                                                                                                                                        ((equal? (send player-shield get-shield) 0) (send player-health lose-health 10) (Bullet-Renderer (rest l)))
                                                                                                                                                                                                                                                                                                                                                                        (else (send player-shield lose-shield 10) (Bullet-Renderer (rest l)))))
                                                                                             ((and (and (> rotation-val 270) (>= 360 rotation-val)) (and (< (send player-c get-xcoord3) (- (send (first l) get-ux) 34)) (> (send player-c get-xcoord1) (- (send (first l) get-ux) 34))) (and (< (send player-c get-ycoord2) (-(send (first l) get-uy)34)) (> (send player-c get-ycoord1) (-(send (first l) get-uy)34)))) (cond
                                                                                                                                                                                                                                                                                                                                                                        ((equal? (send player-health get-health) 0 ) (set! screen-mode 11) (set! current-screen "screen L"))
                                                                                                                                                                                                                                                                                                                                                                        ((equal? (send player-shield get-shield) 0) (send player-health lose-health 10) (Bullet-Renderer (rest l)))
                                                                                                                                                                                                                                                                                                                                                                        (else (send player-shield lose-shield 10) (Bullet-Renderer (rest l)))))
                                                                                          
                                                                                           (else (Bullet-Renderer (rest l)))))))
  






;;Function used top update each individual coordinate by sending the collision% object the update coord function 
(define (update-x-point1 object  center rotate-val x y)
  (send object update-xcoord1 (+ center (-(* x (sin (* rotate-val (/ pi 180)))) (* y (cos (* rotate-val (/ pi 180))))))))
(define (update-x-point2 object  center rotate-val x y)
  (send object update-xcoord2 (+ center (-(* x (sin (* rotate-val (/ pi 180)))) (* y (cos (* rotate-val (/ pi 180))))))))
(define (update-x-point3 object  center rotate-val x y)
  (send object update-xcoord3 (+ center (-(* x (sin (* rotate-val (/ pi 180)))) (* y (cos (* rotate-val (/ pi 180))))))))
(define (update-x-point4 object  center rotate-val x y)
  (send object update-xcoord4 (+ center (-(* x (sin (* rotate-val (/ pi 180)))) (* y (cos (* rotate-val (/ pi 180))))))))

(define (update-y-point1 object center rotate-val x y)
  (send object update-ycoord1 (+ center (+(* x (cos (* rotate-val (/ pi 180)))) (* y (sin (* rotate-val (/ pi 180))))))))
(define (update-y-point2 object center rotate-val x y)
  (send object update-ycoord2 (+ center (+(* x (cos (* rotate-val (/ pi 180)))) (* y (sin (* rotate-val (/ pi 180))))))))
(define (update-y-point3 object center rotate-val x y)
  (send object update-ycoord3 (+ center (+(* x (cos (* rotate-val (/ pi 180)))) (* y (sin (* rotate-val (/ pi 180))))))))
(define (update-y-point4 object center rotate-val x y)
  (send object update-ycoord4 (+ center (+(* x (cos (* rotate-val (/ pi 180)))) (* y (sin (* rotate-val (/ pi 180))))))))


  





;;Screen boundaries Set so that the scrolling function does not fail and the tank stays centered constantly
;;Our screen size 1000w x 800h
(define (screen-boundaries)  
                                                                                                                                                                                                                                                           
       (set! scroll-horiz (*(/ 1 (- virtual-w 1000)) (- (ptcx Tank) 500)))  
       (set! scroll-vert (*(/ 1 (- virtual-h 800)) (- (ptcy Tank) 400)))
       (send canvas scroll  scroll-horiz scroll-vert))
 
      

    
(define (bulletx x angle r)
  (+(ptcx x) (* r (- 0(sin (* angle (/ pi 180)))))))

(define (bullety y angle r)
  (+(ptcy y) (* r (- 0(cos (* angle (/ pi 180)))))))



;;Tank rotate clockwise
(define (rotation-func+)
  (cond
    ((equal? 360 rotation-val) (set! rotation-val 0) (set! rotation-val (+ 5 rotation-val)))
    (else (set! rotation-val (+ 5 rotation-val)))))

;;Tank rotate counter clockwise
(define (rotation-func-)
  (cond
    ((equal? 0 rotation-val) (set! rotation-val 360) (set! rotation-val (- rotation-val 5)))
    (else (set! rotation-val (- rotation-val 5)))))

;;Sets the active cursor as a bullseye
(define game-cursor (make-object cursor% 'bullseye))
  



(define (forward-movement x y rotate)
  (cond
      ((and (>= 90 rotate) (> rotate 0))
               (set! y-speed (- (* (/ 8 90) rotate) 8))
               (set! x-speed (- 0 (* (/ 8 90) rotate))))
      ((and (>= 180 rotate) (> rotate 90))
               (set! y-speed  (* (/ 8 90) (- rotate 90 )))
               (set! x-speed (- (* (/ 8 90) (- rotate 90)) 8 )))
      ((and (>= 270 rotate) (> rotate 180))
               (set! y-speed  (- 8 (* (/ 8 90) (- rotate 180))))
               (set! x-speed    (* (/ 8 90) (- rotate 180))))
      ((and (>= 360 rotate) (> rotate 270))
               (set! y-speed  (- 0(* (/ 8 90) (- rotate 270))))
               (set! x-speed   (- 8(* (/ 8 90) (- rotate 270)))))))
               

(define (ptcx x) (+ ptx (* 1/2 (image-width (circle-align-middle x)))))
(define (ptcy x) (+ pty (* 1/2 (image-height (circle-align-middle x)))))

(define (etcx x) (+ etx (* 1/2 (image-height (circle-align-middle x)))))
(define (etcy x) (+ ety (* 1/2 (image-height (circle-align-middle x)))))




(define (controlx x) (+ ptx (* 1/2 (image-width (circle-align-middle x)))))
(define (controly x) (- pty (* 1/2 (image-height (circle-align-middle x)))))

(define (controlex x) (+ etx (* 1/2 (image-width (circle-align-middle x)))))
(define (controley x) (- ety (* 1/2 (image-height (circle-align-middle x)))))





(define (Angle x y)
  (define a (distance-points (ptcx Tank) (controlx Tank) (ptcy Tank) (controly Tank)))
  (define b (distance-points (ptcx Tank) x (ptcy Tank) y))
  (define c (distance-points (controlx Tank) x (controly Tank) y))
  (cond
    ((> x (ptcx Tank))
     (set! turret-rotation  (- 0 (round (*(cosC a b c) (/ 180 pi))))))
    (else (set! turret-rotation (round (*(cosC a b c) (/ 180 pi)))))))

(define (E-Angle)
  (define a (distance-points (etcx Enemy-Tank) (controlex Enemy-Tank) (etcy Enemy-Tank) (controley Enemy-Tank)))
  (define b (distance-points (etcx Enemy-Tank) (ptcx Tank) (etcy Enemy-Tank) (ptcy Tank)))
  (define c (distance-points (controlex Enemy-Tank) (ptcx Tank) (controley Enemy-Tank) (ptcy Tank)))
  (cond
    ((> (ptcx Tank) (etcx Enemy-Tank))
     (set! enemy-turret-rotation  (- 0 (round (*(cosC a b c) (/ 180 pi))))))
    (else (set! enemy-turret-rotation (round (*(cosC a b c) (/ 180 pi)))))))


 

;;MiniMap function tracks player and enemy position
(define (minimap)
  (cond
    ((equal? map-loading 1)
                    (call-with-values (lambda () (send canvas get-view-start)) (lambda (x y) (render-image MiniMap dc (+ 855 x) (+ 5 y)))) ;;render background png 
                    (call-with-values (lambda () (send canvas get-view-start)) (lambda (x y) (render-image (rectangle 140 140 'outline 'black) dc (+ 855 x) (+ 5 y)))) ;;render a black bounding rectangle for the minmap 
                    (call-with-values (lambda () (send canvas get-view-start)) (lambda (x y) (render-image (circle 2 'solid 'green) dc (+ (+ 855 x) (* (/ 140 2990) (-(ptcx Tank) 505)))  (+ (+ 5 y) (* (/ 140 2990) (-(ptcy Tank) 405)))))) ;; render a green dot as player position 
                    (call-with-values (lambda () (send canvas get-view-start)) (lambda (x y) (render-image (circle 2 'solid 'red) dc (+ (+ 855 x) (* (/ 140 2990) (-(etcx Enemy-Tank) 505)))  (+ (+ 5 y) (* (/ 140 2990) (-(etcy Enemy-Tank) 405))))))))) ;; render a red dot as enemy position 
                    



                                           
;; A clock that decrements a number every 1000ms effectively creating a countdown number
;;Used for round length 
(define Countdown
  (new timer%
       [notify-callback (λ ()
                         (cond
                           ((and (equal? minutes 0) (equal? seconds 0)) (set! screen-mode 0) (send canvas scroll 0 0) (set! minutes 7) (send Countdown stop))
                           ((equal? 0 seconds) (set! minutes (- minutes 1)) (set! seconds 59))
                           (else (set! seconds (- seconds 1)))))]
       [interval 1000]))





;; A clock that will cause the enemy tank to fire every 4 seconds 
(define Enemy-Fire
  (new timer%
       [notify-callback (λ () (Bullet-Storm-E )
                          (play CannFire-2))]
       [interval 4000]))
                          


;;Main Clock responsible for all rendering 
(define Clock (new timer%
                  [notify-callback (λ ()                                                                                                                                                                                                                                                                                                                                
                                     (cond
                                              ((equal? screen-mode 1) ;;screen mode 1 is the loading screen
                                               (send dc draw-bitmap Loading_bot 0 0)
                                               (set! loading-var (+ 30 loading-var)) ;;increments loading var (red bar)
                                               (cond
                                                 ((>= loading-var 700)
                                                  (render-image (rectangle loading-var 100 'solid 'red) dc 150 350) (set! screen-mode 2) (send Countdown start 1000) (send Enemy-Fire start 10000) (send canvas set-cursor game-cursor) (send canvas focus)) ;; once full changes to game mode and starts the other clock with given intervals
                                                 (else (render-image (rectangle loading-var 100 'solid 'red) dc 150 350)))
                                               (send dc draw-bitmap Loading 0 0)
                                               (send canvas swap-bitmaps))
                                              
                                            
                                            
                                         ((equal? screen-mode 2)

                                          
                                          
                                         (send dc draw-bitmap Final-Map 0 0 ) ;; renders the background map
                                         (screen-boundaries)

                                         (render-image (rotate enemy-rotation-val (circle-align-middle Enemy-Tank)) dc etx ety) ;;rendering via dc to canvas of player and enemy 
                                         (render-image (rotate enemy-turret-rotation (circle-align-middle Enemy-Turret)) dc etx ety)
                                         (render-image (rotate rotation-val (circle-align-middle Tank)) dc ptx pty)                                         
                                         (render-image (rotate turret-rotation (circle-align-middle Turret)) dc ptx pty)


                                         
                                         
                                         ;(cond
                                         ;((= RunCount 0)(play Engine-1)(set! RunCount (add1 RunCount)))
                                         ;((= RunCount 180)(set! RunCount 0))
                                         ;(else (set! RunCount (add1 RunCount))))
                                                                                                                          
                                         (Bullet-Renderer Projectile-List) ;; rendering Bullets 
                                         
                                         
                                         (minimap) ;; redering minimap 
                                         (call-with-values (lambda ()(send canvas get-view-start)) (lambda (x y) (send dc draw-bitmap HSB x y)))
                                         (call-with-values (lambda ()(send canvas get-view-start)) (lambda (x y) (render-image (rectangle (* (/ 419 (send player-health get-max-health)) (send player-health get-health)) 45 'solid 'red) dc (+ x 26) (+ y 4))))
                                         (call-with-values (lambda ()(send canvas get-view-start)) (lambda (x y) (render-image (rectangle (* (/ 419 (send player-shield get-max-shield)) (send player-shield get-shield)) 45 'solid 'blue) dc (+ x 26) (+ y 56))))
                                         (call-with-values (lambda () (send canvas get-view-start)) (lambda (x y) (render-image (rectangle (send player-loading get-recharge) 45 'solid 'white) dc (+ x 26) (+ y 752)))) 
                                         (call-with-values (lambda () (send canvas get-view-start)) (lambda (x y) (send dc draw-bitmap HS x y)))
                                         (call-with-values (lambda () (send canvas get-view-start)) (lambda (x y) (render-image current-bullet dc (+(+ x 233) (/(- 44 (image-width current-bullet)) 2)) (+(+ y 752) (/(- 45 (image-height current-bullet)) 2)))))
                                         (call-with-values (lambda () (send canvas get-view-start)) (lambda (x y) (render-image (text (number->string minutes) 40 (color 255 255 255 200)) dc (+ x 490) (+ y 5))))
                                         (call-with-values (lambda () (send canvas get-view-start)) (lambda (x y) (render-image (text ":" 40 (color 255 255 255 200)) dc (+ x 515) (+ y 5))))
                                         (call-with-values (lambda () (send canvas get-view-start)) (lambda (x y) (render-image (text (number->string seconds) 40 (color 255 255 255 200)) dc (+ x 530) (+ y 5))))
                                         ;;Player Gui elements rendering using canvas get-view-start function to offset and align from virtual origin


                                         
                                         ;(render-image (circle 5 'solid 'blue) dc (send player-c get-xcoord1) (send player-c get-ycoord1))
                                         ;(render-image (circle 5 'solid 'green) dc (send player-c get-xcoord2) (send player-c get-ycoord2))
                                        ; (render-image (circle 5 'solid 'white) dc (send player-c get-xcoord3) (send player-c get-ycoord3))
                                         ;(render-image (circle 5 'solid 'purple) dc (send player-c get-xcoord4) (send player-c get-ycoord4))

                                         ;;Were using double buffering this function swaps the front and the back bitmap then clears the new back-bitmap
                                         (send canvas swap-bitmaps)
                                         
                                         
                                     ;;Key-Handler
                                     ;forward movement state rendering 
                                     (when(or (key-down? 'down) (key-down? #\s))
                                              (cond
                                              ((and (equal? cpg 1) (equal? cpw 1)) void)
                                              ((and (equal? cpb 1) (equal? cpp 1)) void)
                                              ((and (equal? cpg 1) (equal? cpp 1)) void)
                                              ((and (equal? cpb 1) (equal? cpw 1)) void)
                                              (else
                                            (inverse-movement)
                                            (forward-movement x-speed y-speed rotation-val)
                                            EngineSound
                                            (sleep/yield 0.001))))
                                           
                                            

                                     
                                     ;down movement state rendering 
                                     (when(or (key-down? 'up) (key-down? #\w))                                                                                          
                                            (movement)
                                            (forward-movement x-speed y-speed rotation-val)
                                       EngineSound
                                            (sleep/yield 0.001))

          
                                     ;;Seperate thread for checking Player and Enemy collision checker
                                     ;;Speeds up calculation and visibly improves performance on my machine 
                                     
                                     (thread (lambda ()
                                         (find-object (ptcx Tank) (ptcy Tank))
                                         (collision-checker player-c collision-list)
                                         (collision-behaviour-player )
                                               
                                         (find-object (etcx Enemy-Tank) (etcy Enemy-Tank))
                                         (collision-checker enemy-c collision-list)
                                         (collision-behaviour-enemy)))
                                         
                                         
                                         
                                     
                                     (thread (lambda ()

                                      (E-Angle)
                                      ;right movement state rendering 
                                     (when(or (key-down? 'right) (key-down? #\d))                                                                                                                                                                                           
                                       (rotation-func-)
                                       (forward-movement x-speed y-speed rotation-val)
                                       EngineSound
                                       (sleep/yield 0.01))

                                             ;left movement state rendering 
                                     (when(or (key-down? 'left) (key-down? #\a))                                                                                                                                                           
                                       (rotation-func+)
                                       (forward-movement x-speed y-speed rotation-val)
                                       EngineSound
                                       (sleep/yield 0.001))
                                       

                                      ;(Bullet-Checker Projectile-List)
                                       (for/list ([i Projectile-List])
                                            (send i x-coordinate)
                                            (send i y-coordinate)
                                            (send i increment-radius))

                                        
                                               
                                       ;;Update player tank x points 
                                       (update-x-point1 player-c (ptcx Tank) rotation-val -40.5 -35)
                                       (update-x-point2 player-c (ptcx Tank) rotation-val -40.5 35)
                                       (update-x-point3 player-c (ptcx Tank) rotation-val 40.5 35 )
                                       (update-x-point4 player-c (ptcx Tank) rotation-val  40.5 -35)
                                       ;;Update player tank y points        
                                       (update-y-point1 player-c (ptcy Tank) rotation-val  -40.5 -35)
                                       (update-y-point2 player-c (ptcy Tank) rotation-val  -40.5  35)
                                       (update-y-point3 player-c (ptcy Tank) rotation-val  40.5  35)
                                       (update-y-point4 player-c (ptcy Tank) rotation-val  40.5  -35)

                                       ;;Update enemy x points 
                                       (update-x-point1 enemy-c (etcx Enemy-Tank) enemy-rotation-val -40.5 -35)
                                       (update-x-point2 enemy-c (etcx Enemy-Tank) enemy-rotation-val -40.5 35)
                                       (update-x-point3 enemy-c (etcx Enemy-Tank) enemy-rotation-val 40.5 35 )
                                       (update-x-point4 enemy-c (etcx Enemy-Tank) enemy-rotation-val  40.5 -35)
                                               
                                       (update-y-point1 enemy-c (etcy Enemy-Tank) enemy-rotation-val  -40.5 -35)
                                       (update-y-point2 enemy-c (etcy Enemy-Tank) enemy-rotation-val  -40.5  35)
                                       (update-y-point3 enemy-c (etcy Enemy-Tank) enemy-rotation-val  40.5  35)
                                       (update-y-point4 enemy-c (etcy Enemy-Tank) enemy-rotation-val  40.5  -35)
                                      

                                        (unless (>= (round (send player-loading get-recharge)) 196)
                                          (send player-loading recharge-reload)))))

                                       
                                       

                                              ((equal? screen-mode 3)
                                        (send dc draw-bitmap (hash-ref hash-screen0 current-screen)  0 0)
                                        (render-image (rectangle 350 50 "solid" "dark gray") dc 300 270)
                                        (render-image (text "Master Sound" 20 "Red") dc 400 270)
                                        (render-image (rectangle RecX 16 "solid" "red") dc 302 302)
                                       ; (pstream-set-volume! new-pstream (/ RecX 346))
                                        (send canvas swap-bitmaps))

                                      ((equal? screen-mode 4)
                                        (send dc draw-bitmap (hash-ref hash-screen0 current-screen)  0 0)
                                        (render-image (text current-new-name 40 "Black") dc 265 380)
                                        
                                        (send canvas swap-bitmaps))

                                      ((equal? screen-mode 5)
                                       (send dc draw-bitmap (hash-ref hash-screen0 current-screen)  0 0)
                                       (cond
                                          ((file-exists? "0profile.txt") (render-image (text (fourth (LoadSave2 "0profile.txt")) 20 "Black") dc (+ 260 5) (+ 247 5)))
                                          (else (render-image (text "No_Data" 20 "Black") dc (+ 260 5) (+ 247 5))))
                                        (cond
                                          ((file-exists? "1profile.txt") (render-image (text (fourth (LoadSave2 "1profile.txt")) 20 "Black") dc (+ 260 5) (+ 290 5)))
                                          (else (render-image (text "No_Data" 20 "Black") dc (+ 260 5) (+ 290 5))))
                                        (cond
                                          ((file-exists? "2profile.txt") (render-image (text (fourth (LoadSave2 "2profile.txt")) 20 "Black") dc (+ 260 5) (+ 334 5)))
                                          (else (render-image (text "No_Data" 20 "Black") dc (+ 260 5) (+ 334 5))))
                                        (cond
                                          ((file-exists? "3profile.txt") (render-image (text (fourth (LoadSave2 "3profile.txt")) 20 "Black") dc (+ 260 5) (+ 377 5)))
                                          (else (render-image (text "No_Data" 20 "Black") dc (+ 260 5) (+ 377 5))))
                                        (cond
                                          ((file-exists? "4profile.txt") (render-image (text (fourth (LoadSave2 "4profile.txt")) 20 "Black") dc (+ 260 5) (+ 420 5)))
                                          (else (render-image (text "No_Data" 20 "Black") dc (+ 260 5) (+ 420 5))))
                                        (cond
                                          ((file-exists? "5profile.txt") (render-image (text (fourth (LoadSave2 "5profile.txt")) 20 "Black") dc (+ 260 5) (+ 460 5)))
                                          (else (render-image (text "No_Data" 20 "Black") dc (+ 260 5) (+ 460 5))))
                                        (cond
                                          ((file-exists? "6profile.txt") (render-image (text (fourth (LoadSave2 "6profile.txt")) 20 "Black") dc (+ 260 5) (+ 500 5)))
                                          (else (render-image (text "No_Data" 20 "Black") dc (+ 260 5) (+ 500 5))))
                                        (cond
                                          ((file-exists? "7profile.txt") (render-image (text (fourth (LoadSave2 "7profile.txt")) 20 "Black") dc (+ 260 5) (+ 545 5)))
                                          (else (render-image (text "No_Data" 20 "Black") dc (+ 260 5) (+ 545 5))))
                                        (cond
                                          ((file-exists? "8profile.txt") (render-image (text (fourth (LoadSave2 "8profile.txt")) 20 "Black") dc (+ 260 5) (+ 583 5)))
                                          (else (render-image (text "No_Data" 20 "Black") dc (+ 260 5) (+ 583 5))))
                                        
                                        (send canvas swap-bitmaps))
                                      
                                      ((equal? screen-mode 6)
                                        (send dc draw-bitmap (hash-ref hash-screen0 current-screen)  0 0)
                                        
                                        
                                        (send canvas swap-bitmaps))

                                      ((equal? screen-mode 7)
                                       (send dc draw-bitmap (hash-ref hash-screen0 current-screen)  0 0)
                                        (cond
                                          ((file-exists? "0profile.txt") (render-image (text (fourth (LoadSave2 "0profile.txt")) 20 "Black") dc (+ 260 5) (+ 247 5)))
                                          (else (render-image (text "No_Data" 20 "Black") dc (+ 260 5) (+ 247 5))))
                                        (cond
                                          ((file-exists? "1profile.txt") (render-image (text (fourth (LoadSave2 "1profile.txt")) 20 "Black") dc (+ 260 5) (+ 290 5)))
                                          (else (render-image (text "No_Data" 20 "Black") dc (+ 260 5) (+ 290 5))))
                                        (cond
                                          ((file-exists? "2profile.txt") (render-image (text (fourth (LoadSave2 "2profile.txt")) 20 "Black") dc (+ 260 5) (+ 334 5)))
                                          (else (render-image (text "No_Data" 20 "Black") dc (+ 260 5) (+ 334 5))))
                                        (cond
                                          ((file-exists? "3profile.txt") (render-image (text (fourth (LoadSave2 "3profile.txt")) 20 "Black") dc (+ 260 5) (+ 377 5)))
                                          (else (render-image (text "No_Data" 20 "Black") dc (+ 260 5) (+ 377 5))))
                                        (cond
                                          ((file-exists? "4profile.txt") (render-image (text (fourth (LoadSave2 "4profile.txt")) 20 "Black") dc (+ 260 5) (+ 420 5)))
                                          (else (render-image (text "No_Data" 20 "Black") dc (+ 260 5) (+ 420 5))))
                                        (cond
                                          ((file-exists? "5profile.txt") (render-image (text (fourth (LoadSave2 "5profile.txt")) 20 "Black") dc (+ 260 5) (+ 460 5)))
                                          (else (render-image (text "No_Data" 20 "Black") dc (+ 260 5) (+ 460 5))))
                                        (cond
                                          ((file-exists? "6profile.txt") (render-image (text (fourth (LoadSave2 "6profile.txt")) 20 "Black") dc (+ 260 5) (+ 500 5)))
                                          (else (render-image (text "No_Data" 20 "Black") dc (+ 260 5) (+ 500 5))))
                                        (cond
                                          ((file-exists? "7profile.txt") (render-image (text (fourth (LoadSave2 "7profile.txt")) 20 "Black") dc (+ 260 5) (+ 545 5)))
                                          (else (render-image (text "No_Data" 20 "Black") dc (+ 260 5) (+ 545 5))))
                                        (cond
                                          ((file-exists? "8profile.txt") (render-image (text (fourth (LoadSave2 "8profile.txt")) 20 "Black") dc (+ 260 5) (+ 583 5)))
                                          (else (render-image (text "No_Data" 20 "Black") dc (+ 260 5) (+ 583 5))))
                                       
                                        
                                 
                                        
                                        
                                        
                                      
                                        
                                        
                                        (send canvas swap-bitmaps))
                                       


                                         ;;if the screen-mode does not have a pre-defined behaviour then a hash-screen is rendered 
                                         (else (send dc draw-bitmap (hash-ref hash-screen0 current-screen)  0 0)
                                         (send canvas swap-bitmaps))))]
                   [interval 30]))
                                      
                                       
                                      

                                     
;;Screen controller
;;A variable keeping track of the current screen that decides which controls (mouse/keyboard) are currently active 
(define screen-mode 0)

                             
;;Function and variable used to control the hover function
(define HovSCouControl 0)
(define (HovSCou x) (cond
                      ((not(equal? x HovSCouControl))(play Button-P1)(set! HovSCouControl x))
                     ))



;;Hover Handler
;; Gets xy position from mouse position relative to the top-left corner 
(define hover-handle (λ (x y)
                       (cond
                         ((equal? 0 screen-mode)
                          (cond
                            ((and (>= x 348) (>= 589 x) (>= y 261) (>= 342 y)) (set! current-screen "screen 0.1")(HovSCou 1))
                            ((and (>= x 348) (>= 589 x) (>= y 347) (>= 428 y)) (set! current-screen "screen 0.2")(HovSCou 1))
                            ((and (>= x 348) (>= 589 x) (>= y 434) (>= 515 y)) (set! current-screen "screen 0.3")(HovSCou 1))
                            ((and (>= x 348) (>= 589 x) (>= y 521) (>= 603 y)) (set! current-screen "screen 0.4")(HovSCou 1))
                            (else (set! current-screen "screen 0.0") (HovSCou 0))))
                         
                         ((equal? 2 screen-mode)
                          (call-with-values (lambda ()(send canvas get-view-start)) (lambda (a b) (Angle (+ x a) (+ y b)))))
                            ((equal? 3 screen-mode)
                          (cond
                            ((and (>= x 360) (>= 600 x) (>= y 559) (>= 639 y)) (set! current-screen "screen 3.1")(HovSCou 1))
                            (else (set! current-screen "screen 3.0") (HovSCou 0))
                            ))
                         
                         ((equal? 4 screen-mode)
                          (cond
                            ((and (>= x 357) (>= 593 x) (>= y 499) (>= 578 y)) (set! current-screen "screen 4.0")(HovSCou 1))
                            (else (set! current-screen "screen 4.1"))
                            ))

                         ((equal? 5 screen-mode)
                          (set! current-screen "screen 5.0"))
                         
                         ((equal? 6 screen-mode)
                          (cond
                            ((and (>= x 200 ) (>= 480 x) (>= y 400) (>= 550 y)) (set! current-screen "screen 6.4")(HovSCou 1))
                            ((and (>= x 520) (>= 800 x) (>= y 400) (>= 550 y)) (set! current-screen "screen 6.3")(HovSCou 1))
                            ((and (>= x 800) (>= 1000 x) (>= y 715) (>= 800 y)) (set! current-screen "screen 6.2")(HovSCou 1))
                            ((and (>= x 800) (>= 1000 x) (>= y 0) (>= 100 y)) (set! current-screen "screen 6.5")(HovSCou 1))
                            ((and (>= x 403) (>= 603 x) (>= y 722) (>= 800 y)) (set! current-screen "screen 6.1")(HovSCou 1))
                            
                            (else (set! current-screen "screen 6.0") (HovSCou 0))))
                         ((equal? 7 screen-mode)
                          (set! current-screen "screen 5.0"))

                         ((equal? 8 screen-mode)
                          (set! current-screen "screen 5.0")))))
                         
                            
                         
                         
                          
;;Left Click Handler
;;Click Handler produces results based on screen-mode 
(define click-handle (λ (x y)
                       (cond
                        ((equal? 0 screen-mode)
                          (cond
                            ((and (>= x 300) (>= 700 x) (>= y 285) (>= 335 y))   (set! screen-mode 4) (set! current-screen "screen 4.0"))
                            ((and (>= x 348) (>= 589 x) (>= y 347) (>= 428 y))   (set! screen-mode 5) (set! current-screen "screen 5.0"))
                            ((and (>= x 348) (>= 589 x) (>= y 434) (>= 515 y)) (set! screen-mode 3) (set! current-screen "screen 3.0"))))
                         ((equal? 2 screen-mode)
                          (cond
                            ((>= (round (send player-loading get-recharge)) 196)
                             (send player-loading reset-recharge)
                          (Bullet-Storm )
                          (println x)
                          (println y)
                          (println (send player-c get-ycoord1))
                          (println (send player-c get-ycoord2))
                          (println (send player-c get-ycoord3))
                          (println (send player-c get-ycoord4))
                          (play CannFire-2))))
                               ((equal? 3 screen-mode)
                          (cond
                            ((and (>= x 302) (>= 648 x) (>= y 302) (>= 318 y)) (set! RecX (- x 302)))
                            ((and (>= x 360) (>= 600 x) (>= y 559) (>= 639 y)) (set! screen-mode 0) (set! current-screen "screen 0.0"))))
                            
                          
                          
                         ((equal? 4 screen-mode)
                          (cond
                            ((and (>= x 260) (>= 695 x) (>= y 375) (>= 427 y)) (set! current-new-name (symbol->string(read))))
                            ((and (>= x 357) (>= 593 x) (>= y 499) (>= 578 y)) (set! screen-mode 5))
                            ))


                           ((equal? 5 screen-mode)(cond
                                                  ((and (>= x 260) (>= 696 x) (>= y 247) (>= 290 y)) (Save-Overwrite 1 0) (set! screen-mode 6) (set! current-screen "screen 6.0"))
                                                  ((and (>= x 260) (>= 696 x) (>= y 290) (>= 334 y)) (Save-Overwrite 1 1) (set! screen-mode 6) (set! current-screen "screen 6.0"))
                                                  ((and (>= x 260) (>= 696 x) (>= y 334) (>= 375 y)) (Save-Overwrite 1 2) (set! screen-mode 6) (set! current-screen "screen 6.0"))
                                                  ((and (>= x 260) (>= 696 x) (>= y 377) (>= 420 y)) (Save-Overwrite 1 3) (set! screen-mode 6) (set! current-screen "screen 6.0"))
                                                  ((and (>= x 260) (>= 696 x) (>= y 420) (>= 460 y)) (Save-Overwrite 1 4) (set! screen-mode 6) (set! current-screen "screen 6.0"))
                                                  ((and (>= x 260) (>= 696 x) (>= y 460) (>= 500 y)) (Save-Overwrite 1 5) (set! screen-mode 6) (set! current-screen "screen 6.0"))
                                                  ((and (>= x 260) (>= 696 x) (>= y 500) (>= 542 y)) (Save-Overwrite 1 6) (set! screen-mode 6) (set! current-screen "screen 6.0"))
                                                  ((and (>= x 260) (>= 696 x) (>= y 545) (>= 583 y)) (Save-Overwrite 1 7) (set! screen-mode 6) (set! current-screen "screen 6.0"))
                                                  ((and (>= x 260) (>= 696 x) (>= y 583) (>= 622 y)) (Save-Overwrite 1 8) (set! screen-mode 6) (set! current-screen "screen 6.0"))))
                         
                         ((equal? 11 screen-mode)
                          (set! current-screen "screen 0.0") (set! screen-mode 0))
                         
                          ((equal? 6 screen-mode)
                           (cond
                             ((and (>= x 800) (>= 1000 x) (>= y 715) (>= 800 y)) (set! screen-mode 0)(set! current-screen "screen 0.0"))
                             ((and (>= x 200 ) (>= 480 x) (>= y 400) (>= 550 y)) (set! current-screen "screen ND") (set! screen-mode 11))
                            ((and (>= x 520) (>= 800 x) (>= y 400) (>= 550 y)) (set! current-screen "screen ND") (set! screen-mode 11))
                            ((and (>= x 800) (>= 1000 x) (>= y 0) (>= 100 y)) (set! current-screen "screen ND") (set! screen-mode 11))
                            ((and (>= x 403) (>= 603 x) (>= y 722) (>= 800 y)) (set! screen-mode 1) (stop))))

                          ((equal? 7 screen-mode)(cond
                                                  ((and (>= x 260) (>= 696 x) (>= y 247) (>= 290 y)) (LoadProfile "0profile.txt")  (set! screen-mode 6) (set! current-screen "screen 6.0"))
                                                  ((and (>= x 260) (>= 696 x) (>= y 290) (>= 334 y)) (LoadProfile "1profile.txt")  (set! screen-mode 6) (set! current-screen "screen 6.0"))
                                                  ((and (>= x 260) (>= 696 x) (>= y 334) (>= 375 y)) (LoadProfile "2profile.txt")  (set! screen-mode 6) (set! current-screen "screen 6.0"))
                                                  ((and (>= x 260) (>= 696 x) (>= y 377) (>= 420 y)) (LoadProfile "3profile.txt")  (set! screen-mode 6) (set! current-screen "screen 6.0"))
                                                  ((and (>= x 260) (>= 696 x) (>= y 420) (>= 460 y)) (LoadProfile "4profile.txt")  (set! screen-mode 6) (set! current-screen "screen 6.0"))
                                                  ((and (>= x 260) (>= 696 x) (>= y 460) (>= 500 y)) (LoadProfile "5profile.txt")  (set! screen-mode 6) (set! current-screen "screen 6.0"))
                                                  ((and (>= x 260) (>= 696 x) (>= y 500) (>= 542 y)) (LoadProfile "6profile.txt")  (set! screen-mode 6) (set! current-screen "screen 6.0"))
                                                  ((and (>= x 260) (>= 696 x) (>= y 545) (>= 583 y)) (LoadProfile "7profile.txt")  (set! screen-mode 6) (set! current-screen "screen 6.0"))
                                                  ((and (>= x 260) (>= 696 x) (>= y 583) (>= 622 y)) (LoadProfile "8profile.txt")  (set! screen-mode 6) (set! current-screen "screen 6.0"))))

                          ((equal? 8 screen-mode)
                           (cond
                             ((and (>= x 800) (>= 1000 x) (>= y 715) (>= 800 y)) (set! screen-mode 2)(set! current-screen "screen 0.0"))))

                          ((equal? 9 screen-mode)
                           (cond
                             ((and (>= x 800) (>= 1000 x) (>= y 715) (>= 800 y)) (set! screen-mode 2)(set! current-screen "screen 0.0"))))

                          
                             
                                                 

                                                                                          )))

;;Right Click Handler
(define right-click-handler (λ ()
                             (cond
                                   ((equal? screen-mode 2)(play LG-1))))) ;; On Right Click plays following sound (non pstream rsound)
                                 
                          
                          
                                                                              
                        
                       



;;my canvas subclass takes the super class of animated canvas and is modified 

(define my-canvas%
  (class animated-canvas%
    (inherit get-dc swap-bitmaps)

                         
    ;;Mouse event handler checks the event type and applies a function to it 
    (define/override (on-event e)
      (let ([event-type (send e get-event-type)])
        (cond
          ((equal? event-type 'motion) (hover-handle (send e get-x) (send e get-y)))
          ((equal? event-type 'left-down) (click-handle (send e get-x) (send e get-y)))
          ((equal? event-type 'right-down)(right-click-handler)))))
         
      
    (define/override (on-char canvas)  
      (define key-pressed     (send canvas get-key-code)) ;get key pressed
      (define key-released    (send canvas get-key-release-code)) ;get key released 
      
      (when (eq? key-released 'press) ;key is pressed
        (key-down! key-pressed))
      
      (when (eq? key-pressed 'release) ;key is not pressed   
        (key-up! key-released))
      
      ;fullscreen function currently just gives a black border around the screen
      (when (eq? key-released #\f) (cond
                                        ((equal? #t (send Game-window is-fullscreened?)) (send Game-window fullscreen #f))
                                        ((equal? #f (send Game-window is-fullscreened?)) (send Game-window fullscreen #t))))
      ;;spawn minimap
      (when (eq? key-released #\m) (cond
                                     ((equal? 1 map-loading) (set! map-loading 0))
                                     ((equal? 0 map-loading) (set! map-loading 1))))
      ;;Bullet swapper 
      (when (eq? key-released #\1) (set! current-bullet standard-bullet))
      (when (eq? key-released #\2) (set! current-bullet rocket))
      (when (eq? key-released #\3) (set! current-bullet Emp))
      (when (eq? key-released #\4) (set! current-bullet laser-blast)))
     
    (super-instantiate () (style (list  'hscroll 'vscroll )))))




;;my-frame% subclass modified to provide on close functions that will disable sound and stop the clocks
(define my-frame%
  (class frame%
    (super-new)
    (define/augment (on-close)
      (stop) (send Clock stop)
      (send Enemy-Fire stop)
      (send Countdown stop))
    (define/override (show show?)
           (unless show?
             (stop) (send Clock stop))
           (super show show?))))
    
    

;Create a frame to house canvas in 
(define Game-window (new my-frame%
                         [label "Game"]
                         [min-width 1000]
                         [min-height 800]
                         [stretchable-width #f]
                         [stretchable-height #f]))


;Canvas instantiation and drawing context creation
;-------------------------------------------------------

; canvas instance with parent and stretchable parameters                                                                             
(define canvas (new my-canvas%
                    [parent Game-window]
                    [min-width 1000]
                    [min-height 800]
                    [stretchable-width #f]
                    [stretchable-height #f]))
                    
;drawing context for the canvas responsible for rending 
(define dc (send canvas get-dc))


;;Stops the other clocks so they can be started with a later call
(send Countdown stop)
(send Enemy-Fire stop)


;;initialises scrollbars creating virtual space 
(send canvas init-auto-scrollbars virtual-w virtual-h scroll-horiz scroll-vert)
;;initialise automatic scrollbars 
(send canvas show-scrollbars #f #f) ;;hide scrollbars 
;(System-Check) ;;Check if system is Windows (needed for rsound compatibility)
(send Game-window show #t) ;Show frame start program

(define CurrentPlayer (list current-new-name ))






