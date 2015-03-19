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

(defn- new-building-card [color number actions]
  (-> {}
      (assoc :card-type :building)
      (assoc :color color)
      (assoc :number number)
      (into actions)))

(defn building-cards-blue []
  [])

(defn building-cards-yellow []
  [])

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

;     ___
;    / _ \__ _ _ __ ___   ___
;   / /_\/ _` | '_ ` _ \ / _ \
;  / /_\\ (_| | | | | | |  __/
;  \____/\__,_|_| |_| |_|\___|

(defn setup []
  (let [tiles (group-by (fn [tile]
                          (if (<= (:number tile) 3)
                            :setup
                            :remaining)) (building-tiles))]
    {:tiles (zipmap (shuffle [[-1 1] [0 1] [1 1]
                              [-1 0] [0 0] [1 0]
                              [-1 -1] [0 -1] [1 -1]]) (:setup tiles))}))


