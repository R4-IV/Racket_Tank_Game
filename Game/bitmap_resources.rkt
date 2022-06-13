#lang send-exp racket/gui
(require rsound)

;Map Element 
(define Final-Map (make-object bitmap% "Resources/Screens/Final-Game-Map.png" 'png))


;MiniMap
(define MiniMap (make-object bitmap% "Resources/Screens/MiniMap.png" 'png/alpha))




;Tank Bases
(define Heavy-Tank (make-object bitmap% "Resources/Tanks/Heavy-Tank.png" 'png/alpha))
(define Heavy-Armored-Tank (make-object bitmap% "Resources/Tanks/Heavy-Armored-Tank.png" 'png/alpha))
(define Medium-Tank (make-object bitmap% "Resources/Tanks/Medium-Tank.png" 'png/alpha))
(define Medium-Armored-Tank (make-object bitmap% "Resources/Tanks/Medium-Armored-Tank.png" 'png/alpha))
(define Small-Tank (make-object bitmap% "Resources/Tanks/Small-Tank.png" 'png/alpha))
(define Small-Armored-Tank (make-object bitmap% "Resources/Tanks/Small-Armored-Tank.png" 'png/alpha))

;Tank Turrets
(define Buster-Heavy (make-object bitmap% "Resources/Tanks/Buster-Heavy-Turret.png" 'png/alpha))
(define Assualt (make-object bitmap% "Resources/Tanks/Full-Frontal-Assualt.png" 'png/alpha))
(define Heavy-Battery (make-object bitmap% "Resources/Tanks/Artilery-Battery-Heavy-Turret.png" 'png/alpha))


;Health and Shield
(define HSB (make-object bitmap% "Resources/Screens/Updated-HSRB.png" 'png/alpha))
(define HS (make-object bitmap% "Resources/Screens/HSRR.png" 'png/alpha))

;Drops
(define Health-Pack (make-object bitmap% "Resources/Screens/Healthup.png" 'png/alpha))
(define Shield-Pack (make-object bitmap% "Resources/Screens/Shield-Pack.png" 'png/alpha))



;Title screen
(define Main-title-0 (make-object bitmap% "Resources/Screens/S0.png" 'png/alpha))
(define Main-title-1 (make-object bitmap% "Resources/Screens/S1.png" 'png/alpha))
(define Main-title-2 (make-object bitmap% "Resources/Screens/S2.png" 'png/alpha))
(define Main-title-3 (make-object bitmap% "Resources/Screens/S3.png" 'png/alpha))
(define Main-title-4 (make-object bitmap% "Resources/Screens/S4.png" 'png/alpha))



;Loading Screen
(define Loading (make-object bitmap% "Resources/Screens/Loading.png" 'png/alpha))
(define Loading_bot (make-object bitmap% "Resources/Screens/Loading-Screen-BotLayer_.png" 'png/alpha))

;Settings screen
(define Settings-0 (make-object bitmap% "Resources/Screens/Set0.png" 'png/alpha))
(define Settings-1 (make-object bitmap% "Resources/Screens/Set1.png" 'png/alpha))

;LoadGame
(define LoadGame (make-object bitmap% "Resources/Screens/LoadGameScreen.png" 'png/alpha))

;NewGame
(define NewGame-1 (make-object bitmap% "Resources/Screens/NewGameName.png" 'png/alpha))
(define NewGame-2 (make-object bitmap% "Resources/Screens/NewGameName2.png" 'png/alpha))

;Ready Screen
(define NotReady(make-object bitmap% "Resources/Screens/Ready Screen 1.png" 'png/alpha))
(define ReadyGo (make-object bitmap% "Resources/Screens/Ready Screen 2.png" 'png/alpha))

;SkillTree
(define SkillTree1 (make-object bitmap% "Resources/Screens/Skill Tree.png" 'png/alpha))
;Settings screen
(define Agho-0 (make-object bitmap% "Resources/Screens/SettingsPage0.png" 'png/alpha))
(define Agho-1 (make-object bitmap% "Resources/Screens/SettingsPage1.png" 'png/alpha))

;Sounds

(define Button-P1 (rs-read  "Resources/Sounds/ButtonSwitch.wav"))
(define Button-C1 (rs-read  "Resources/Sounds/ButtonClick.wav"))
(define CannFire-1 (rs-read  "Resources/Sounds/CannonFire.wav"))
(define CannFire-2 (rs-read  "Resources/Sounds/155mm.wav"))
(define Boom-1 (rs-read  "Resources/Sounds/Splosion.wav"))
(define MG-1 (rs-read  "Resources/Sounds/FutureMachineGun.wav"))
(define LG-1 (rs-read  "Resources/Sounds/Lazer.wav"))
(define Music-1 (rs-read  "Resources/Sounds/1BadApplezBeat.wav"))
(define Music-2 (rs-read  "Resources/Sounds/1DoomedBeat.wav"))
(define Engine-1 (rs-read  "Resources/Sounds/Tank Idle.wav"))
(define Movement-1 (rs-read  "Resources/Sounds/Tank Engine.wav"))
(define CannMove-1 (rs-read  "Resources/Sounds/Swivel1.wav"))
(define Repair-1 (rs-read "Resources/Sounds/Repair.wav"))
(define End-of-the-World (rs-read "Resources/Sounds/Megumin Splosion!.wav"))
(define Glad (rs-read "Resources/Sounds/Gladiator.wav"))

;Skill Tree
(define SkillTree (make-object bitmap% "Resources/Screens/Skill Tree.png" 'png/alpha))

;VICTORY!
(define Victory (make-object bitmap% "Resources/Screens/VictoryScreen.png" 'png/alpha))
;DEFEAT!
(define Defeat (make-object bitmap% "Resources/Screens/FATL" 'png/alpha))
;Bullet

(define standard-bullet (make-object bitmap% "Resources/Bullets/Bullet01.png" 'png/alpha))
(define laser-blast (make-object bitmap% "Resources/Bullets/laser-blast.png" 'png/alpha))
(define rocket (make-object bitmap% "Resources/Bullets/Rocket.png" 'png/alpha))
(define Emp (make-object bitmap% "Resources/Bullets/Emp.png" 'png/alpha))

;;Screen 6 conectivity
(define screen-6.0 (make-object bitmap% "Resources/Screens/screen6.0Initial.png" 'png/alpha))
(define screen-6.1 (make-object bitmap% "Resources/Screens/screen6.1Beta.png" 'png/alpha))
(define screen-6.2 (make-object bitmap% "Resources/Screens/screen6.2Back.png" 'png/alpha))
(define screen-6.3 (make-object bitmap% "Resources/Screens/screen-6.3Join.png" 'png/alpha))
(define screen-6.4 (make-object bitmap% "Resources/Screens/screen6.4Host.png" 'png/alpha))
(define screen-6.5 (make-object bitmap% "Resources/Screens/screen6.5Skills.png" 'png/alpha))

;;Screen no feature
(define screenNF (make-object bitmap% "Resources/Screens/screenNoFeature.png"))



(provide (all-defined-out))
