;; vim: fdm=marker:et:sw=2:fdl=0:
;; title: Bit Path
;; author: Saud (https://saud.wtf)
;; desc: Puzzle game.
;; site: https://saud.wtf
;; license: MIT License
;; version: 0.1
;; script: fennel
;; strict: true

; Constants {{{

(local UP    0)
(local DOWN  1)
(local LEFT  2)
(local RIGHT 3)

(local MOVE-SPD 20)
(local END-TILE 5)
(local ADD-TILE 6)
(local SUB-TILE 22)

; }}}

; Macros & Utils {{{

(macro enum! [...]
  "Create 1-indexed enums for given array of keys.

  Usage:
    (enum! E1 E2 E3)
  Compiles down to:
    (local [E1 E2 E3] [1 2 3])"
  `(local ,[...] ,(icollect [i (ipairs [...])] i)))

(macro --! [val]
  "Like x--, only works with table fields and vars."
  `(set ,val (- ,val 1)))

(macro ++! [val]
  "Like x++, only works with table fields and vars."
  `(set ,val (+ ,val 1)))

; TODO turn this into macro?
(fn state-machine [states-tbl]
  "State machine.
  Use with `enum!`."
  (var current (. states-tbl 1))
  (fn [...]
    (let [next (current ...)]
      (match (type next)
        :number   (set current (. states-tbl next))
        :function (set current next)))
    next))

(fn lerp [a b mu]
  "Classic linear interpolation."
  (+ (* a (- 1 mu)) (* b mu)))

(fn init-list [max]
  "Creates a sequential list of numbers from 1 to `max`."
  (let [list []]
    (for [i 1 max]
      (table.insert list i))
    list))

(fn ticker [interval]
  "Returns true every time `interval` ticks have passed"
  (var ticks 0)
  (fn []
    (set ticks (+ ticks 1))
    (if (> ticks interval)
      (do
        (set ticks 0)
        (values true ticks))
      (values false ticks))))

(fn oscillator [full ondur]
  "Oscillates (flips?) between true and false given duration and on duration.
  This uses `time`, so only works when called every frame.
  TODO find out what this is called in gamedev jargon."
  (< (% (time) full) ondur))

(fn lerp-ticker [interval]
  "Same as ticker but returns lerped value 0 -> 1, and then always returns true."
  (var ticks 0)
  (fn []
    (if (> ticks interval)
      (values true 1.0)
      (do
        (set ticks (+ ticks 1))
        (values false (/ ticks interval))))))

(fn generator [list]
  "Loops over items of list."
  (var idx 1)
  (fn []
    (let [reset? (>= idx (# list))]
      (if reset?
        (set idx 1)
        (set idx (+ idx 1)))
      (values (. list idx) reset?))))

(fn list-loop [list ?interval]
  "Loops over list item every time `interval`(default 1) ticks have passed."
  (var cur-item (. list 1))
  (let [is-time? (ticker (or ?interval 1))
        next-item (generator list)]
    (fn []
      (if (is-time?)
        (let [next (next-item)]
          (set cur-item next)))
      cur-item)))

(fn shadow-print [text x y colfg colbg scale]
  "Print text with shadow."
  (print text (+ x 2) (+ y 2) colbg false scale)
  (print text x       y       colfg false scale))

(fn read-arrows []
  "Returns direction if any arrow key has just been pressed, else nil."
  (if (btnp UP)    UP
      (btnp DOWN)  DOWN
      (btnp LEFT)  LEFT
      (btnp RIGHT) RIGHT
      nil))

(fn make-screen-shake [duration intensity]
  "Shake screen using RAM display xy offset method."
  (let [tick (ticker duration)]
    (fn []
      (let [done? (tick)]
        (if done?
          (do
            (memset 0x3FF9 0 2)
            true)
          (do
            (poke 0x3FF9 (math.random (- intensity) intensity))
            (poke (+ 0x3FF9 1) (math.random (- intensity) intensity))
            false))))))

(fn make-spr-shake [duration intensity]
  (let [tick (ticker duration)]
    (fn []
      (let [done? (tick)]
        (if done?
          (values true 0 0)
          (values
            false
            (math.random (- intensity) intensity)
            (math.random (- intensity) intensity)))))))

; }}}

; Main {{{

;; Start and end swipe transitions

(local swipe-enter
  (let [transition (ticker 90)]
    (fn []
      (let [(done? amt) (transition)]
        (rect 0 0 240 (lerp 0 136 (/ amt 40)) 5)
        done?))))

(local swipe-exit
  (let [transition (ticker 90)]
    (fn []
      (let [(done? amt) (transition)]
        (rect 0 0 240 (lerp 136 0 (/ amt 40)) 5)
        done?))))

(fn play-error-sfx []
  (sfx 0 nil 10))

(fn play-move-sfx []
  (sfx 1 nil 14))

(fn play-inc-sfx []
  (sfx 2 nil 10))

(fn play-dec-sfx []
  (sfx 3 nil 10))

(fn play-end-sfx []
  (sfx 16 nil 50))

;; Global objects

(local Player {:sprid 16
               :spr-loop [16 17]
               :flip-spr 0
               :x 0
               :y 0
               :relx 0
               :rely 0
               :last-move-dir nil
               :move-step (fn [])})

(local Levels {:current 1
               ; start (top left) pos of current level
               :current-pos { :x nil :y nil}
               ; Remaining movement counts
               :current-mvt { :up nil :down nil :left nil :right nil}
               ; Level positions and movement counts
               :lv-data [{ :x 60  :y 0  :up 0 :down 0 :left 0 :right 5}
                         { :x 90  :y 0  :up 2 :down 1 :left 0 :right 4}
                         { :x 120 :y 0  :up 3 :down 3 :left 2 :right 4}
                         { :x 150 :y 0  :up 2 :down 1 :left 0 :right 3}
                         { :x 180 :y 0  :up 3 :down 4 :left 4 :right 4}
                         { :x 210 :y 0  :up 4 :down 3 :left 2 :right 4}
                         { :x 0   :y 34 :up 2 :down 3 :left 2 :right 6}
                         { :x 30  :y 34 :up 4 :down 2 :left 1 :right 3}
                         { :x 60  :y 34 :up 1 :down 2 :left 0 :right 5}
                         { :x 90  :y 34 :up 1 :down 3 :left 0 :right 3}
                         { :x 120 :y 34 :up 5 :down 6 :left 1 :right 4}
                         { :x 150 :y 34 :up 1 :down 2 :left 2 :right 7}
                         { :x 180 :y 34 :up 4 :down 4 :left 1 :right 8}]
               :sprid 1})

(local Hud {})

(local Battery { :x 0 :y 0})

;; Methods..?

(fn Player.set-pos [x y]
  (set Player.x x)
  (set Player.y y)
  ; Screen(pixel) pos = map pos * 8
  (set Player.relx (* x 8))
  (set Player.rely (* y 8)))

(set Player.draw (let [next-spr (list-loop Player.spr-loop 20)]
                   (fn []
                     (spr (next-spr) Player.relx Player.rely 0 1 Player.flip-spr))))

(fn Player.set-move-pos [dir]
  (set Player.last-move-dir dir)
  (let [next-val (lerp-ticker MOVE-SPD)
        lerpval (match dir
                  UP    (fn [v] (values Player.x (lerp Player.y (- Player.y 1) v)))
                  DOWN  (fn [v] (values Player.x (lerp Player.y (+ Player.y 1) v)))
                  LEFT  (fn [v] (values (lerp Player.x (- Player.x 1) v) Player.y))
                  RIGHT (fn [v] (values (lerp Player.x (+ Player.x 1) v) Player.y)))]

    (set Player.move-step (fn []
                            (let [(done? v) (next-val)
                                  (rx ry) (lerpval v)]
                              (values done? (* rx 8) (* ry 8))))))

  (match dir LEFT (set Player.flip-spr 1)
             RIGHT (set Player.flip-spr 0)))

(fn Player.move []
  (let [(done? relx rely) (Player.move-step)]
    (if done?
      (do
        (Player.set-pos (// relx 8) (// rely 8))
        true)
      (do
        (set Player.relx relx)
        (set Player.rely rely)
        false))))

(fn Levels.draw []
  (map
    Levels.current-pos.x
    Levels.current-pos.y
    30 17
    0 0
    -1 1
    (fn [id]
      ; we will draw player & battery manually
      (match id
        Player.sprid Levels.sprid
        END-TILE     Levels.sprid
        _            id))))

(fn Levels.load [lv]
  (let [lvdata (. Levels.lv-data lv)]
    (set Levels.current lv)
    (set Levels.current-pos.x lvdata.x)
    (set Levels.current-pos.y lvdata.y)
    (set Levels.current-mvt.up lvdata.up)
    (set Levels.current-mvt.down lvdata.down)
    (set Levels.current-mvt.left lvdata.left)
    (set Levels.current-mvt.right lvdata.right))
  ; draw map once to find out player & battery coords
  (map Levels.current-pos.x Levels.current-pos.y 30 17 0 0 -1 1 (fn [id px py]
                                                                  (if (= id Player.sprid)
                                                                      ; px is map absolute so convert back to screen coords
                                                                      (Player.set-pos (- px Levels.current-pos.x) (- py Levels.current-pos.y))
                                                                      (= id END-TILE)
                                                                      (do
                                                                        (set Battery.x (* (- px Levels.current-pos.x) 8))
                                                                        (set Battery.y (* (- py Levels.current-pos.y) 8)))))))

(fn Levels.can-move? [dir]
  ; TODO need to extract this into a function
  (let [(tx ty) (match dir
                  UP (values Player.x (- Player.y 1))
                  DOWN (values Player.x (+ Player.y 1))
                  LEFT (values (- Player.x 1) Player.y)
                  RIGHT (values (+ Player.x 1) Player.y))
        sprid (mget (+ tx Levels.current-pos.x) (+ ty Levels.current-pos.y))
        mvt-count (match dir
                    UP Levels.current-mvt.up
                    DOWN Levels.current-mvt.down
                    LEFT Levels.current-mvt.left
                    RIGHT Levels.current-mvt.right)
        steppable? (match sprid
                     1  true
                     2  true
                     3  true
                     5  true
                     6  true
                     16 true
                     22 (> mvt-count 1)
                     _  false)]
    (and steppable? (> mvt-count 0))))

(fn Levels.update-mvt-count [dir]
  (match dir
    UP    (--! Levels.current-mvt.up)
    DOWN  (--! Levels.current-mvt.down)
    LEFT  (--! Levels.current-mvt.left)
    RIGHT (--! Levels.current-mvt.right)))

(fn Levels.inc-last-move []
  (match Player.last-move-dir
     UP (++! Levels.current-mvt.up)
     DOWN (++! Levels.current-mvt.down)
     LEFT (++! Levels.current-mvt.left)
     RIGHT (++! Levels.current-mvt.right))
  nil)

(fn Levels.dec-last-move []
  (match Player.last-move-dir
     UP (--! Levels.current-mvt.up)
     DOWN (--! Levels.current-mvt.down)
     LEFT (--! Levels.current-mvt.left)
     RIGHT (--! Levels.current-mvt.right))
  nil)

(fn Levels.mvt-complete? []
  (= (+ Levels.current-mvt.up Levels.current-mvt.down Levels.current-mvt.left Levels.current-mvt.right) 0))

(fn Levels.is-end-tile? [px py]
  (let [tile (mget (+ px Levels.current-pos.x) (+ py Levels.current-pos.y))]
    (= tile END-TILE)))

(fn Levels.is-add-tile? [px py]
  (let [tile (mget (+ px Levels.current-pos.x) (+ py Levels.current-pos.y))]
    (= tile ADD-TILE)))

(fn Levels.is-sub-tile? [px py]
  (let [tile (mget (+ px Levels.current-pos.x) (+ py Levels.current-pos.y))]
    (= tile SUB-TILE)))

(fn Hud.draw-btn [sprid xpos val]
  (if (= val 0)
    (do
      (spr (+ sprid 16) xpos 110 0 2)
      (print val (+ xpos 5) 100 4))
    (do
      (spr sprid xpos 110 0 2)
      (print val (+ xpos 5) 100 2))))

(fn Hud.draw []
  (print (.. "Level " Levels.current) 100 20 2)
  (Hud.draw-btn 10 60  Levels.current-mvt.left)
  (Hud.draw-btn 7  90  Levels.current-mvt.up)
  (Hud.draw-btn 8  120 Levels.current-mvt.right)
  (Hud.draw-btn 9  150 Levels.current-mvt.down))

(set Battery.draw (let [next-spr (list-loop [32 33 34 35 36 36] 16)]
                    (fn []
                      (spr (next-spr) Battery.x Battery.y 0))))

(local scene-state
  (let [LV-START      1
        IDLE          2
        MOVING        3
        SCREENSHAKE   4
        LV-END        5
        GAME-END      6
        shakescreen   (make-screen-shake 10 1)
        draw-entities (fn []
                        (Levels.draw)
                        (Battery.draw)
                        (Player.draw)
                        (Hud.draw))
        transition    (ticker 90)]

    (state-machine
      { LV-START (fn []
                  (draw-entities)
                  (when (swipe-exit)
                    IDLE))

        IDLE (fn []
               (draw-entities)

               (let [move-dir (read-arrows)]
                 (if move-dir
                   ; can move and have dir power left
                   (if (Levels.can-move? move-dir)
                     (do
                       (Player.set-move-pos move-dir)
                       (Levels.update-mvt-count move-dir)
                       (play-move-sfx)
                       MOVING)
                     (do
                       (play-error-sfx)
                       SCREENSHAKE))
                   ; Reset level if Z is pressed
                   (if (btnp 4)
                     (do
                       (Levels.load Levels.current)
                       (play-error-sfx)
                       SCREENSHAKE)))))

        MOVING (fn []
                 (draw-entities)

                 (let [moved? (Player.move)]
                   (when moved?
                     (if
                       (Levels.is-end-tile? Player.x Player.y)
                       (if (Levels.mvt-complete?)
                         (do
                           (play-end-sfx)
                           LV-END)
                         (do
                           (play-error-sfx)
                           SCREENSHAKE))
                       (do
                         (if
                           (Levels.is-add-tile? Player.x Player.y)
                           (do
                             (Levels.inc-last-move)
                             (play-inc-sfx))
                           (Levels.is-sub-tile? Player.x Player.y)
                           (do
                             (Levels.dec-last-move)
                             (play-dec-sfx)))
                         IDLE)))))

        SCREENSHAKE (fn []
                      (draw-entities)
                      (if (shakescreen)
                        IDLE))

        LV-END (fn []
                   (fn []
                     (draw-entities)
                     (when (swipe-enter)
                       ; all levels completed
                       (if (= Levels.current (# Levels.lv-data))
                         GAME-END
                         (do
                           (Levels.load (+ Levels.current 1))
                           LV-START)))))

        GAME-END (fn []
                   (cls 5)
                   (print "THE END" 100 40)
                   (print "Thanks for playing!" 70 70)
                   (if (oscillator 600 300)
                     (print "Press Z to play again" 62 100 2))
                   (when (btnp 4)
                      (Levels.load 1)
                      LV-START))})))
                   


(local draw-title
  (let [get-scroll-pos (list-loop (init-list 40) 2)]
    (fn []
      (let [scroll-pos (- (get-scroll-pos))]
         (map 0 0 35 22 scroll-pos scroll-pos 14)
         (shadow-print "Bit Path" 48 22 3 6 3)
         ; blink
         (if (oscillator 600 300)
           (print "Press Z to Start" 72 94 2))))))

(local draw-tutorial
  (fn []
    (cls 5)
    (print "   ^" 60 10)
    (print "<-    ->   -    Move" 60 20)
    (print "   v" 60 30)
    (print "   Z       -    Reset" 60 50)
    (print "Power is picked up only when energy is 0" 10 80)
    (if (oscillator 600 300)
      (print "Z - Continue" 78 110 2))))

(local game-state
  (let [TITLE    1
        TUTORIAL 2
        INGAME   3]
    (state-machine
      { TITLE (fn []
                (draw-title)

                ; Z pressed
                (when (btnp 4)
                  (play-move-sfx)
                  (fn []
                    (draw-title)
                    (when (swipe-enter)
                      (Levels.load 1)
                      TUTORIAL))))

        TUTORIAL (fn []
                   (draw-tutorial)
                   (when (btnp 4)
                     INGAME))

        INGAME (fn []
                  (scene-state)

                  nil)})))

(set _G.TIC
  (fn []
    (cls)
    (game-state)))

; }}}

; Metadata {{{

;; <TILES>
;; 001:8333333334444445344444453444444534444445344444453444444535555556
;; 002:8333333334444445344444453454444534344545344455453444334535555556
;; 003:8333333334444445344455453444534534443445344444453444444535555556
;; 004:3333333323333336223333662223366622225666222555662255555625555555
;; 005:4422222244001113440551134405511344055163440551634405516344333333
;; 006:8333533334435445344354453332433355546555344354453443544535535556
;; 007:0222222021133116213334162313414621134116211341162114411606666660
;; 008:0222222021113116211113162333334623344446211114162111411606666660
;; 009:0222222021133116211331162113411623134146213344162114411606666660
;; 010:0222222021131116213111162333334623344446214111162114111606666660
;; 015:0000000000033000003553000356653003566530003553000003300000000000
;; 016:0566666006244470064646700646467006444470067777700033550000011000
;; 017:0000000005666660062444700646467006464670064444700677777000011000
;; 022:8333333334444445344444453433334534666645344444453444444535555556
;; 023:0333333031144116314445163414515631145116311451163115511606666660
;; 024:0333333031114116311114163444445634455556311115163111511606666660
;; 025:0333333031144116311441163114511634145156314455163115511606666660
;; 026:0333333031141116314111163444445634455556315111163115111606666660
;; 031:0000000003000030004004000006600000066000004004000300003000000000
;; 032:0000000000011000001111000015610000156100001561000011110000000000
;; 033:0000000000011000001111000015610000156100001341000011110000000000
;; 034:0000000000011000001111000015610000134100001341000011110000000000
;; 035:0000000000011000001111000013410000134100001341000011110000000000
;; 036:0000000000011000001111000013410000134100001341000011110000000000
;; </TILES>

;; <MAP>
;; 002:0000f000000000f100000000f000000000f100000000f000000000f100000000f0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 006:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000005000000000000000000000000000000000000000000000000000000000000000101010000000000000000000000000
;; 007:0000f100000000f000000000f100000000f000000000f100000000f000000000f1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000001030101010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010100110100000000000000000000000000000000000000000000000000000000000100050000000000000000000000000
;; 008:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001102010105000000000000000000000000000000000000000000000000000000050000000000000000000000000000000000000000000000000000000000100500010000000000000000000000000000000000000000000000010301010500000000000000000000000000000000000000000000000000060001000600000000000000000000000000000000000000000000000000000000110601060000000000000000000000000
;; 009:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000110102010000000000000000000000000000000000000000000000000000000001010102010000000000000000000000000000000000000000000000001000000100000000000000000000000000000000000000000000000000010101010100000000000000000000000000000000000000000000000000000000000100010000000000000000000000000
;; 010:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010106010200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000101010000000000000000000000000
;; 012:0000f000000000f100000000f000000000f100000000f000000000f100000000f0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 017:0000f100000000f000000000f100000000f000000000f100000000f000000000f1000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 040:000000000000000000000000000000000000000000000000000000000000000000000000000000000000101060601000000000000000000000000000000000000000000000000000101061100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001010101000000000000000000000000000000000000000000000000000006061100000000000000000000000000000000000000000000000000000000010610010600000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 041:000000000000000000000000001010100000000000000000000000000000000000000000000000000000100010005000000000000000000000000000000000000000000000000001106110105000000000000000000000000000000000000000000000000000001010101000000000000000000000000000000000000000000000000000001061601000000000000000000000000000000000000000000000000000606160615000000000000000000000000000000000000000000000000000101060106010611010000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 042:000000000000000000000001101060105000000000000000000000000000000000000000000000000000100160101000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001060611000000000000000000000000000000000000000000000000000001060611000000000000000000000000000000000000000000000000000610161610000000000000000000000000000000000000000000000000000011061001061005061000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 043:000000000000000000000000001010000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000110105000000000000000000000000000000000000000000000000000000110105000000000000000000000000000000000000000000000000000006160000000000000000000000000000000000000000000000000000000000010106000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; </MAP>

;; <WAVES>
;; 000:00000000ffffffff00000000ffffffff
;; 001:0123456789abcdeffedcba9876543210
;; 002:0123456789abcdef0123456789abcdef
;; </WAVES>

;; <SFX>
;; 000:0000000000000000000000000000f000f000f000f000f000f000000000000000000000000000f000f000f000f000f000f000f000f000f000f000f000124000000000
;; 001:010001000100010001000100010001004100f100f100f100f100f100f100f100f100f100f100f100f100f100f100f100f100f100f100f100f100f10042b000000000
;; 002:404e205f00c100d200e000e000b0f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000500000000000
;; 003:00e200e210b1209e408d506060007000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000f000400000000000
;; 008:00002001400f5001600e7001800f9000a000b000600080009000a000b000b00090009000b000c000c000c000c000a000c000d000e000e000f000f000200000000000
;; 009:730083009300a300b300c300d300e300e300f300f300f300f300f300f300f300f300f300f300f300f300f300f300f300f300f300f300f300f300f300200000000000
;; 016:a123712371236123618361836183218321832182218221f121f021ff21ff21fe21fe21fe21fe31fe31ff31f131f131f231f351f371f381f391f3c1f3304000000000
;; </SFX>

;; <PATTERNS>
;; 000:900096800096000090000090600096000090000090d00096000090000090000090000090900096800096000000000090600096000000000090d00096000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; 001:700006000000000000000000b00006000000000000000000700006b00086000000b00086000080700086000000b00086000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
;; </PATTERNS>

;; <TRACKS>
;; 000:000240000040000000000000000000000000000000000000000000000000000000000000000000000000000000000000007230
;; </TRACKS>

;; <PALETTE>
;; 000:40484c000000eee9d5d4bfbaa871835e405e231b45132533011c1cffffffffffffffffffffffffffffffffffffffffff
;; </PALETTE>

