(ns ginkgopolis.core)

;         _   _ _
;   _   _| |_(_) |___
;  | | | | __| | / __|
;  | |_| | |_| | \__ \
;   \__,_|\__|_|_|___/


;     _        _   _
;    /_\   ___| |_(_) ___  _ __  ___
;   //_\\ / __| __| |/ _ \| '_ \/ __|
;  /  _  \ (__| |_| | (_) | | | \__ \
;  \_/ \_/\___|\__|_|\___/|_| |_|___/

(defn- on-action [actionType funcs]
  {:when   :on-action
   :filter (fn [action _game]
             (= (:action-type action) actionType))
   :apply  (fn [action game]
             (reduce (fn [game0 func] (func action game0)) game funcs))})

(defn- on-endgame [func]
  {:when  :on-endgame
   :apply func})

(defn- resource-gain [_action game]
  (throw (UnsupportedOperationException. "Not implemented")))

(defn- point-gain [_action game]
  (throw (UnsupportedOperationException. "Not implemented")))

(defn- tile-gain [_action game]
  (throw (UnsupportedOperationException. "Not implemented")))


(defn- tile-color-bonus [color]
  (fn [& args] (throw (UnsupportedOperationException. "Not implemented"))))

(defn- tile-height-bonus [filter bonus]
  (fn [& args] (throw (UnsupportedOperationException. "Not implemented"))))

(defn- card-type-bonus [cardType bonus]
  (fn [& args] (throw (UnsupportedOperationException. "Not implemented"))))

(defn- card-color-bonus [cardColor bonus]
  (fn [& args] (throw (UnsupportedOperationException. "Not implemented"))))

(defn- point-bonus [bonus]
  (fn [& args] (throw (UnsupportedOperationException. "Not implemented"))))

(defn new-action
  ([card]
   (new-action card {}))
  ([card params]
   {:what   :action
    :card   card
    :params params}))

;    ___              _
;   / __\__ _ _ __ __| |___
;  / /  / _` | '__/ _` / __|
; / /__| (_| | | | (_| \__ \
; \____/\__,_|_|  \__,_|___/

(defn building-tiles []
  (let [numbers (range 1 21)]
    (reduce (fn [acc num]
              (-> acc
                  (conj {:number num :color :blue})
                  (conj {:number num :color :red})
                  (conj {:number num :color :yellow})))
            '() numbers)))

(defn urbanization-cards []
  (map #(assoc % :card-type :urbanization)
       [
        {:letter :A}
        {:letter :B}
        {:letter :C}
        {:letter :D}
        {:letter :E}
        {:letter :F}
        {:letter :G}
        {:letter :H}
        {:letter :I}
        {:letter :J}
        {:letter :K}
        {:letter :L}]))

(defn- new-building-card [color number action]
  (-> {}
      (assoc :card-type :building)
      (assoc :color color)
      (assoc :number number)
      (assoc :action action)))

(defn building-cards-blue []
  [
   (new-building-card :blue 1 (on-action :exploit [tile-gain]))
   (new-building-card :blue 2 (on-action :urbanization [tile-gain]))
   (new-building-card :blue 3 (on-action :floor-construction [tile-gain]))
   (new-building-card :blue 4 (on-action :exploit [tile-gain]))
   (new-building-card :blue 5 (on-action :urbanization [tile-gain]))
   (new-building-card :blue 6 (on-action :floor-construction [tile-gain]))
   (new-building-card :blue 7 (on-action :exploit [tile-gain point-gain]))
   (new-building-card :blue 8 (on-action :urbanization [tile-gain point-gain]))
   (new-building-card :blue 9 (on-action :floor-construction [tile-gain point-gain]))
   (new-building-card :blue 10 (on-endgame (tile-color-bonus :blue)))
   (new-building-card :blue 11 (on-endgame (tile-color-bonus :red)))
   (new-building-card :blue 12 (on-endgame (tile-color-bonus :yellow)))
   (new-building-card :blue 13 (on-endgame (tile-color-bonus :blue)))
   (new-building-card :blue 14 (on-endgame (tile-height-bonus #(or (= 1 %) (= 2 %)) 1)))
   (new-building-card :blue 15 (on-endgame (tile-height-bonus #(<= 3 %) 3)))
   (new-building-card :blue 16 (on-endgame (card-type-bonus :exploit 2)))
   (new-building-card :blue 17 (on-endgame (card-type-bonus :urbanization 2)))
   (new-building-card :blue 18 (on-endgame (card-type-bonus :floor-construction 2)))
   (new-building-card :blue 19 (on-endgame (card-color-bonus :blue 2)))
   (new-building-card :blue 20 (on-endgame (point-bonus 9)))
   ])

(defn building-cards-yellow []
  [
   (new-building-card :yellow 1 (on-action :exploit [point-gain]))
   (new-building-card :yellow 2 (on-action :urbanization [point-gain]))
   (new-building-card :yellow 3 (on-action :floor-construction [point-gain]))
   (new-building-card :yellow 4 (on-action :exploit [point-gain]))
   (new-building-card :yellow 5 (on-action :urbanization [point-gain]))
   (new-building-card :yellow 6 (on-action :floor-construction [point-gain]))
   (new-building-card :yellow 7 (on-action :exploit [point-gain point-gain]))
   (new-building-card :yellow 8 (on-action :urbanization [point-gain point-gain]))
   (new-building-card :yellow 9 (on-action :floor-construction [point-gain point-gain]))
   (new-building-card :yellow 10 (on-endgame (tile-color-bonus :yellow)))
   (new-building-card :yellow 11 (on-endgame (tile-color-bonus :red)))
   (new-building-card :yellow 12 (on-endgame (tile-color-bonus :blue)))
   (new-building-card :yellow 13 (on-endgame (tile-color-bonus :yellow)))
   (new-building-card :yellow 14 (on-endgame (tile-height-bonus #(or (= 1 %) (= 2 %)) 1)))
   (new-building-card :yellow 15 (on-endgame (tile-height-bonus #(<= 3 %) 3)))
   (new-building-card :yellow 16 (on-endgame (card-type-bonus :exploit 2)))
   (new-building-card :yellow 17 (on-endgame (card-type-bonus :urbanization 2)))
   (new-building-card :yellow 18 (on-endgame (card-type-bonus :floor-construction 2)))
   (new-building-card :yellow 19 (on-endgame (card-color-bonus :yellow 2)))
   (new-building-card :yellow 20 (on-endgame (point-bonus 9)))
   ])

(defn building-cards-red []
  [
   (new-building-card :red 1 (on-action :exploit [resource-gain]))
   (new-building-card :red 2 (on-action :urbanization [resource-gain]))
   (new-building-card :red 3 (on-action :floor-construction [resource-gain]))
   (new-building-card :red 4 (on-action :exploit [resource-gain]))
   (new-building-card :red 5 (on-action :urbanization [resource-gain]))
   (new-building-card :red 6 (on-action :floor-construction [resource-gain]))
   (new-building-card :red 7 (on-action :exploit [resource-gain point-gain]))
   (new-building-card :red 8 (on-action :urbanization [resource-gain point-gain]))
   (new-building-card :red 9 (on-action :floor-construction [resource-gain point-gain]))
   (new-building-card :red 10 (on-endgame (tile-color-bonus :red)))
   (new-building-card :red 11 (on-endgame (tile-color-bonus :yellow)))
   (new-building-card :red 12 (on-endgame (tile-color-bonus :blue)))
   (new-building-card :red 13 (on-endgame (tile-color-bonus :red)))
   (new-building-card :red 14 (on-endgame (tile-height-bonus #(or (= 1 %) (= 2 %)) 1)))
   (new-building-card :red 15 (on-endgame (tile-height-bonus #(<= 3 %) 3)))
   (new-building-card :red 16 (on-endgame (card-type-bonus :exploit 2)))
   (new-building-card :red 17 (on-endgame (card-type-bonus :urbanization 2)))
   (new-building-card :red 18 (on-endgame (card-type-bonus :floor-construction 2)))
   (new-building-card :red 19 (on-endgame (card-color-bonus :red 2)))
   (new-building-card :red 20 (on-endgame (point-bonus 9)))
   ])

(defn building-cards []
  (-> []
      (into (building-cards-blue))
      (into (building-cards-yellow))
      (into (building-cards-red))
      ))

(defn- new-character
  ([color initialItems action]
   (new-character color initialItems action nil))
  ([color initialItems action groupId]
   {:card-type    :character
    :color        color
    :action       action
    :initialItems initialItems
    :groupId      groupId}))

(defn character-cards []
  [
   ; ---
   (new-character :red
                  [:resource :point :tile]
                  (on-action :urbanization [resource-gain])
                  1)
   (new-character :yellow
                  [:resource :resource :point :tile]
                  (on-action :floor-construction [point-gain])
                  1)
   (new-character :blue
                  [:resource]
                  (on-action :floor-construction [tile-gain])
                  1)
   ; ---
   (new-character :red
                  [:tile]
                  (on-action :floor-construction [resource-gain])
                  2)
   (new-character :yellow
                  [:resource :resource :point :tile]
                  (on-action :floor-construction [point-gain])
                  2)
   (new-character :blue
                  [:resource :resource :point]
                  (on-action :exploit [tile-gain])
                  2)
   ; ---
   (new-character :red
                  [:resource :point :tile]
                  (on-action :exploit [resource-gain])
                  3)
   (new-character :yellow
                  [:resource :resource :point :point :tile]
                  (on-action :urbanization [point-gain])
                  3)
   (new-character :blue
                  [:resource :resource :point]
                  (on-action :urbanization [tile-gain])
                  3)
   ; ---
   (new-character :red
                  [:resource :point :tile]
                  (on-action :urbanization [resource-gain])
                  4)
   (new-character :yellow
                  [:resource :resource :point :point :tile]
                  (on-action :urbanization [point-gain])
                  4)
   (new-character :blue
                  [:resource]
                  (on-action :floor-construction [tile-gain])
                  4)
   ; ---
   (new-character :red
                  [:resource :point :tile]
                  (on-action :exploit [resource-gain])
                  5)
   (new-character :yellow
                  [:resource :resource :point :point :tile]
                  (on-action :exploit [point-gain])
                  5)
   (new-character :blue
                  [:resource]
                  (on-action :floor-construction [tile-gain])
                  5)
   ; ---
   (new-character :red
                  [:resource :point :tile]
                  (on-action :urbanization [resource-gain])
                  6)
   (new-character :yellow
                  [:resource :resource :point :point :tile]
                  (on-action :exploit [point-gain])
                  6)
   (new-character :blue
                  [:resource :resource :point]
                  (on-action :exploit [tile-gain])
                  6)
   ; ---
   (new-character :blue
                  [:resource :resource :point]
                  (on-action :urbanization [tile-gain]))
   (new-character :blue
                  [:resource :resource :point]
                  (on-action :urbanization [tile-gain]))
   (new-character :red
                  [:tile]
                  (on-action :floor-construction [resource-gain]))
   (new-character :red
                  [:tile]
                  (on-action :floor-construction [resource-gain]))
   (new-character :red
                  [:tile]
                  (on-action :floor-construction [resource-gain]))
   (new-character :red
                  [:resource :point :tile]
                  (on-action :exploit [resource-gain]))
   (new-character :red
                  [:resource :point :tile]
                  (on-action :exploit [resource-gain]))
   (new-character :blue
                  [:resource :resource :point]
                  (on-action :exploit [tile-gain]))
   (new-character :red
                  [:resource :point :tile]
                  (on-action :urbanization [resource-gain]))
   ])

;     ___
;    / _ \__ _ _ __ ___   ___
;   / /_\/ _` | '_ ` _ \ / _ \
;  / /_\\ (_| | | | | | |  __/
;  \____/\__,_|_| |_| |_|\___|

;
; Tile board and coordinate
;
; +.....+.....+.....+.....+.....+
; :     :     :     :     :     :
; :-2,+2:-1,+2: 0,+2:+1,+2:+2,+2:
; :     :     :     :     :     :
; +.....+=====+=====+=====+.....+
; :     |     :     :     |     :
; :-2,+1|-1,+1: 0,+1:+1,+1|+2,+1:
; :     |     :     :     |     :
; +.....+.....+.....+.....+.....+
; :     |     :     :     |     :
; :-2,0 |-1,0 : 0,0 :+1,0 |+2,0 :
; :     |     :     :     |     :
; +.....+.....+.....+.....+.....+
; :     |     :     :     |     :
; :-2,-1|-1,-1: 0,-1:+1,-1|+2,-1:
; :     |     :     :     |     :
; +.....+=====+=====+=====+.....+
; :     :     :     :     :     :
; :-2,-2:-1,-2: 0,-2:+1,-2:+2,-2:
; :     :     :     :     :     :
; +.....+.....+.....+.....+.....+
;
;

(defn prepare-tiles [conf]
  (let [nbPlayers (:nbPlayers conf)
        buildingTiles ((:buildingTiles conf))
        shuffleFn (:shuffleFn conf)
        tiles (group-by (fn [tile]
                          (if (<= (:number tile) 3)
                            :setup
                            :remaining)) buildingTiles)
        remaining-tiles (shuffleFn (:remaining tiles))
        remaining-tiles (if (or (= 2 nbPlayers) (= 3 nbPlayers))
                          (drop 6 remaining-tiles)
                          remaining-tiles)
        board (zipmap (shuffleFn [[-1 1] [0 1] [1 1]
                                [-1 0] [0 0] [1 0]
                                [-1 -1] [0 -1] [1 -1]]) (:setup tiles))]
    {:city                 board
     :tiles-general-supply remaining-tiles}))

(defn prepare-urbanization-markers [conf]
  {:urbanization-markers {:a [-1 +2] :b [0 +2] :c [+1 +2]
                          :d [+2 +1]
                          :e [+2 0]
                          :f [+2 -1]
                          :i [-1 -2] :h [0 -2] :g [+1 -2]
                          :l [-2 +1]
                          :k [-2 0]
                          :j [-2 -1]}})

(defn prepare-deck [conf]
  (let [urbanizationCards ((:urbanizationCards conf))
        buildingCards ((:buildingCards conf))]
    {:deck ((:shuffleFn conf) (-> []
                                  (into urbanizationCards)
                                  (into (filter #(<= (:number %) 3) buildingCards))))}))


(defn prepare-characters [conf]
  (let [nbPlayers (:nbPlayers conf)
        characterCards ((:characterCards conf))
        characterCards (group-by :groupId characterCards)
        groupIds (remove nil? (keys characterCards))
        groupIds ((:shuffleFn conf) groupIds)
        groupIds (take nbPlayers groupIds)
        characterCardsDistribution (map #(get characterCards %) groupIds)
        players (range 1 (inc nbPlayers))]
    {:players (zipmap players characterCardsDistribution)}))

(defn default-conf []
  {:nbPlayers         2
   :buildingTiles     building-tiles
   :buildingCards     building-cards
   :urbanizationCards urbanization-cards
   :characterCards    character-cards
   :shuffleFn         shuffle})


(defn setup
  ([]
   (setup (default-conf)))
  ([conf]
   (let [adjustedConf (into (default-conf) conf)]
     (-> {}
         (into (prepare-tiles adjustedConf))
         (into (prepare-urbanization-markers adjustedConf))
         (into (prepare-deck adjustedConf))
         (into (prepare-characters adjustedConf))
         ))))
