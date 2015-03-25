(ns ginkgopolis.core
  (:import (java.util Random)
           (clojure.lang IPersistentMap PersistentVector)))

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

(defn on-action [actionType funcs]
  {:when   :on-action
   :filter (fn [action _game]
             (= (:action-type action) actionType))
   :apply  (fn [action game]
             (reduce (fn [game0 func] (func action game0)) game funcs))})

(defn on-endgame [func]
  {:when  :on-endgame
   :apply func})

(defn resource-gain [_action game]
  (throw (UnsupportedOperationException. "Not implemented")))

(defn point-gain [_action game]
  (throw (UnsupportedOperationException. "Not implemented")))

(defn tile-gain [_action game]
  (throw (UnsupportedOperationException. "Not implemented")))


(defn- tile-color-bonus [color]
  (fn [& args] (throw (UnsupportedOperationException. "Not implemented"))))

(defn- tile-height-bonus [requiredHeights bonus]
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

(defprotocol Card
  (card-color [this])
  (card-type [this])
  (card-number [this])
  (card-letter [this]))

(defrecord building-tile-id [color number]
  Card
  (card-color [this] (:color this))
  (card-type [_this] :tile)
  (card-number [this] (:number this))
  (card-letter [_this] nil))

(defrecord urbanization-card-id [letter]
  Card
  (card-color [_this] nil)
  (card-type [_this] :urbanization)
  (card-number [_this] nil)
  (card-letter [this] (:letter this)))

(defrecord building-card-id [color number]
  Card
  (card-color [this] (:color this))
  (card-type [_this] :building)
  (card-number [this] (:number this))
  (card-letter [_this] nil))

(defrecord character-id [id color]
  Card
  (card-color [this] (:color this))
  (card-type [_this] :character)
  (card-number [_this] nil)
  (card-letter [_this] nil))



;;



(defn new-building-tile [dst [color num]]
  (assoc dst (->building-tile-id color num) {}))

(defn building-tiles []
  (let [numbers (range 1 21)]
    (reduce (fn [acc num]
              (-> acc
                  (new-building-tile [:blue num])
                  (new-building-tile [:red num])
                  (new-building-tile [:yellow num])))
            {} numbers)))

(defn- new-urbanization-card [dst letter]
  (assoc dst (->urbanization-card-id letter) {}))

(defn urbanization-cards []
  (reduce new-urbanization-card {} [:A :B :C :D :E :F :G :H :I :J :K :L]))

(defn- new-building-card [dst color number action]
  (assoc dst (->building-card-id color number) {:action action}))

(defn building-cards-blue []
  (-> {}
      (new-building-card :blue 1 '(on-action :exploit [tile-gain]))
      (new-building-card :blue 2 '(on-action :urbanization [tile-gain]))
      (new-building-card :blue 3 '(on-action :floor-construction [tile-gain]))
      (new-building-card :blue 4 '(on-action :exploit [tile-gain]))
      (new-building-card :blue 5 '(on-action :urbanization [tile-gain]))
      (new-building-card :blue 6 '(on-action :floor-construction [tile-gain]))
      (new-building-card :blue 7 '(on-action :exploit [tile-gain point-gain]))
      (new-building-card :blue 8 '(on-action :urbanization [tile-gain point-gain]))
      (new-building-card :blue 9 '(on-action :floor-construction [tile-gain point-gain]))
      (new-building-card :blue 10 '(on-endgame (tile-color-bonus :blue)))
      (new-building-card :blue 11 '(on-endgame (tile-color-bonus :red)))
      (new-building-card :blue 12 '(on-endgame (tile-color-bonus :yellow)))
      (new-building-card :blue 13 '(on-endgame (tile-color-bonus :blue)))
      (new-building-card :blue 14 '(on-endgame (tile-height-bonus [1 2] 1)))
      (new-building-card :blue 15 '(on-endgame (tile-height-bonus [3] 3)))
      (new-building-card :blue 16 '(on-endgame (card-type-bonus :exploit 2)))
      (new-building-card :blue 17 '(on-endgame (card-type-bonus :urbanization 2)))
      (new-building-card :blue 18 '(on-endgame (card-type-bonus :floor-construction 2)))
      (new-building-card :blue 19 '(on-endgame (card-color-bonus :blue 2)))
      (new-building-card :blue 20 '(on-endgame (point-bonus 9)))
      ))

(defn building-cards-yellow []
  (-> {}
      (new-building-card :yellow 1 '(on-action :exploit [point-gain]))
      (new-building-card :yellow 2 '(on-action :urbanization [point-gain]))
      (new-building-card :yellow 3 '(on-action :floor-construction [point-gain]))
      (new-building-card :yellow 4 '(on-action :exploit [point-gain]))
      (new-building-card :yellow 5 '(on-action :urbanization [point-gain]))
      (new-building-card :yellow 6 '(on-action :floor-construction [point-gain]))
      (new-building-card :yellow 7 '(on-action :exploit [point-gain point-gain]))
      (new-building-card :yellow 8 '(on-action :urbanization [point-gain point-gain]))
      (new-building-card :yellow 9 '(on-action :floor-construction [point-gain point-gain]))
      (new-building-card :yellow 10 '(on-endgame (tile-color-bonus :yellow)))
      (new-building-card :yellow 11 '(on-endgame (tile-color-bonus :red)))
      (new-building-card :yellow 12 '(on-endgame (tile-color-bonus :blue)))
      (new-building-card :yellow 13 '(on-endgame (tile-color-bonus :yellow)))
      (new-building-card :yellow 14 '(on-endgame (tile-height-bonus #(or (= 1 %) (= 2 %)) 1)))
      (new-building-card :yellow 15 '(on-endgame (tile-height-bonus #(<= 3 %) 3)))
      (new-building-card :yellow 16 '(on-endgame (card-type-bonus :exploit 2)))
      (new-building-card :yellow 17 '(on-endgame (card-type-bonus :urbanization 2)))
      (new-building-card :yellow 18 '(on-endgame (card-type-bonus :floor-construction 2)))
      (new-building-card :yellow 19 '(on-endgame (card-color-bonus :yellow 2)))
      (new-building-card :yellow 20 '(on-endgame (point-bonus 9)))
      ))

(defn building-cards-red []
  (-> {}
      (new-building-card :red 1 '(on-action :exploit [resource-gain]))
      (new-building-card :red 2 '(on-action :urbanization [resource-gain]))
      (new-building-card :red 3 '(on-action :floor-construction [resource-gain]))
      (new-building-card :red 4 '(on-action :exploit [resource-gain]))
      (new-building-card :red 5 '(on-action :urbanization [resource-gain]))
      (new-building-card :red 6 '(on-action :floor-construction [resource-gain]))
      (new-building-card :red 7 '(on-action :exploit [resource-gain point-gain]))
      (new-building-card :red 8 '(on-action :urbanization [resource-gain point-gain]))
      (new-building-card :red 9 '(on-action :floor-construction [resource-gain point-gain]))
      (new-building-card :red 10 '(on-endgame (tile-color-bonus :red)))
      (new-building-card :red 11 '(on-endgame (tile-color-bonus :yellow)))
      (new-building-card :red 12 '(on-endgame (tile-color-bonus :blue)))
      (new-building-card :red 13 '(on-endgame (tile-color-bonus :red)))
      (new-building-card :red 14 '(on-endgame (tile-height-bonus #(or (= 1 %) (= 2 %)) 1)))
      (new-building-card :red 15 '(on-endgame (tile-height-bonus #(<= 3 %) 3)))
      (new-building-card :red 16 '(on-endgame (card-type-bonus :exploit 2)))
      (new-building-card :red 17 '(on-endgame (card-type-bonus :urbanization 2)))
      (new-building-card :red 18 '(on-endgame (card-type-bonus :floor-construction 2)))
      (new-building-card :red 19 '(on-endgame (card-color-bonus :red 2)))
      (new-building-card :red 20 '(on-endgame (point-bonus 9)))
      ))

(defn building-cards []
  (-> {}
      (into (building-cards-blue))
      (into (building-cards-yellow))
      (into (building-cards-red))))

(defn new-character
  ([dst id color initialItems action]
   (new-character dst id color initialItems action nil))
  ([dst id color initialItems action groupId]
   (assoc dst (->character-id id color) {:action       action
                                         :initialItems initialItems
                                         :groupId      groupId})))

(defn character-cards []
  (-> {}
      (new-character 1 :red
                     [:resource :point :tile]
                     '(on-action :urbanization [resource-gain])
                     1)
      (new-character 2 :yellow
                     [:resource :resource :point :tile]
                     '(on-action :floor-construction [point-gain])
                     1)
      (new-character 3 :blue
                     [:resource]
                     '(on-action :floor-construction [tile-gain])
                     1)
      ; ---
      (new-character 4 :red
                     [:tile]
                     '(on-action :floor-construction [resource-gain])
                     2)
      (new-character 5 :yellow
                     [:resource :resource :point :tile]
                     '(on-action :floor-construction [point-gain])
                     2)
      (new-character 6 :blue
                     [:resource :resource :point]
                     '(on-action :exploit [tile-gain])
                     2)
      ; ---
      (new-character 7 :red
                     [:resource :point :tile]
                     '(on-action :exploit [resource-gain])
                     3)
      (new-character 8 :yellow
                     [:resource :resource :point :point :tile]
                     '(on-action :urbanization [point-gain])
                     3)
      (new-character 9 :blue
                     [:resource :resource :point]
                     '(on-action :urbanization [tile-gain])
                     3)
      ; ---
      (new-character 10 :red
                     [:resource :point :tile]
                     '(on-action :urbanization [resource-gain])
                     4)
      (new-character 11 :yellow
                     [:resource :resource :point :point :tile]
                     '(on-action :urbanization [point-gain])
                     4)
      (new-character 12 :blue
                     [:resource]
                     '(on-action :floor-construction [tile-gain])
                     4)
      ; ---
      (new-character 13 :red
                     [:resource :point :tile]
                     '(on-action :exploit [resource-gain])
                     5)
      (new-character 14 :yellow
                     [:resource :resource :point :point :tile]
                     '(on-action :exploit [point-gain])
                     5)
      (new-character 15 :blue
                     [:resource]
                     '(on-action :floor-construction [tile-gain])
                     5)
      ; ---
      (new-character 16 :red
                     [:resource :point :tile]
                     '(on-action :urbanization [resource-gain])
                     6)
      (new-character 17 :yellow
                     [:resource :resource :point :point :tile]
                     '(on-action :exploit [point-gain])
                     6)
      (new-character 18 :blue
                     [:resource :resource :point]
                     '(on-action :exploit [tile-gain])
                     6)
      ; ---
      (new-character 19 :blue
                     [:resource :resource :point]
                     '(on-action :urbanization [tile-gain]))
      (new-character 20 :blue
                     [:resource :resource :point]
                     '(on-action :urbanization [tile-gain]))
      (new-character 21 :red
                     [:tile]
                     '(on-action :floor-construction [resource-gain]))
      (new-character 22 :red
                     [:tile]
                     '(on-action :floor-construction [resource-gain]))
      (new-character 23 :red
                     [:tile]
                     '(on-action :floor-construction [resource-gain]))
      (new-character 24 :red
                     [:resource :point :tile]
                     '(on-action :exploit [resource-gain]))
      (new-character 25 :red
                     [:resource :point :tile]
                     '(on-action :exploit [resource-gain]))
      (new-character 26 :blue
                     [:resource :resource :point]
                     '(on-action :exploit [tile-gain]))
      (new-character 27 :red
                     [:resource :point :tile]
                     '(on-action :urbanization [resource-gain]))
      ))

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
        buildingTileIds (keys (:buildingTiles conf))
        shuffleFn (:shuffleFn conf)
        tiles (group-by (fn [id]
                          (if (<= (card-number id) 3)
                            :setup
                            :remaining)) buildingTileIds)
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
  (let [urbanizationCards (keys (:urbanizationCards conf))
        buildingCards (keys (:buildingCards conf))]
    {:deck ((:shuffleFn conf) (-> []
                                  (into urbanizationCards)
                                  (into (filter (fn [k] (<= (card-number k) 3)) buildingCards))))}))

(defn prepare-resources [conf]
  (let [nbPlayers (:nbPlayers conf)
        nbResourcePerPlayer (cond (= 2 nbPlayers) 25
                                  (= 3 nbPlayers) 20
                                  (= 4 nbPlayers) 18
                                  (= 5 nbPlayers) 16
                                  :else (throw (UnsupportedOperationException. (str "Invalid number of players: '" nbPlayers "'"))))
        players (range 1 (inc nbPlayers))]
    {:resources-general-supply (zipmap players (repeat nbPlayers nbResourcePerPlayer))}))

(defn predefined-characters-shuffle [conf]
  (let [nbPlayers (:nbPlayers conf)
        characterCards (:characterCards conf)
        characterCardIds (group-by #(get-in characterCards [% :groupId]) (keys characterCards))
        groupIds (remove nil? (keys characterCardIds))
        groupIds ((:shuffleFn conf) groupIds)
        groupIds (take nbPlayers groupIds)
        characterCardsDistribution (map #(get characterCardIds %) groupIds)
        players (range 1 (inc nbPlayers))]
    (zipmap players characterCardsDistribution)))


(defn prepare-players [conf]
  (let [nbPlayers (:nbPlayers conf)
        characterCardsPerPlayer ((:charactersDistributionFn conf) conf)
        players (range 1 (inc nbPlayers))
        playerFn (fn [playerId]
                   {:characters      (get characterCardsPerPlayer playerId)
                    :new-hand-tokens 2
                    :resources       0
                    :tiles           []
                    :points          0})
        playersData (reduce (fn [ps playerId]
                              (assoc ps playerId (playerFn playerId)))
                            {} players)]
    {:players playersData}))

(defn default-settings []
  {:nbPlayers                 2
   :charactersDistributionFn  predefined-characters-shuffle
   :buildingTilesProvider     building-tiles
   :buildingCardsProvider     building-cards
   :urbanizationCardsProvider urbanization-cards
   :characterCardsProvider    character-cards
   :random                    (Random. 42)
   :shuffleFn                 shuffle})


(defn setup
  ([]
   (setup (default-settings)))
  ([settings]
   (let [adjustedSettings (into (default-settings) settings)
         nbPlayers (:nbPlayers adjustedSettings)
         conf {:nbPlayers                nbPlayers
               :buildingTiles            ((:buildingTilesProvider adjustedSettings))
               :buildingCards            ((:buildingCardsProvider adjustedSettings))
               :urbanizationCards        ((:urbanizationCardsProvider adjustedSettings))
               :characterCards           ((:characterCardsProvider adjustedSettings))
               :charactersDistributionFn (:charactersDistributionFn adjustedSettings)
               :random                   (:random adjustedSettings)
               :shuffleFn                (:shuffleFn adjustedSettings)}
         emptyHands (reduce (fn [m pId] (assoc m pId [])) {} (range 1 (inc nbPlayers)))]
     (-> {}
         (assoc :conf conf)
         (assoc :round 0)
         (assoc :players-hands emptyHands)
         (into (prepare-tiles conf))
         (into (prepare-urbanization-markers conf))
         (into (prepare-deck conf))
         (into (prepare-resources conf))
         (into (prepare-players conf))
         ))))

;         _
;   _ __ | | __ _ _   _
;  | '_ \| |/ _` | | | |
;  | |_) | | (_| | |_| |
;  | .__/|_|\__,_|\__, |
;  |_|            |___/

(defn- update-player-with-resource [game playerId]
  (-> game
      (update-in [:players playerId :resources] inc)
      (update-in [:resources-general-supply playerId] dec)))

(defn- update-player-with-tile [game playerId]
  (let [headTile (first (:tiles-general-supply game))]
    (-> game
        (update-in [:players playerId :tiles] conj headTile)
        (update-in [:tiles-general-supply] rest))))

(defn- update-player-with-point [game playerId]
  (-> game
      (update-in [:players playerId :points] inc)))

(defn- update-player-with-item [game playerId item]
  (cond (= item :resource) (update-player-with-resource game playerId)
        (= item :tile) (update-player-with-tile game playerId)
        (= item :point) (update-player-with-point game playerId)
        :else game))

(defn initial-items-of [game characterId]
  (get-in game [:conf :characterCards characterId :initialItems]))

(defn take-initial-items-for-player [game playerId]
  (let [player (get-in game [:players playerId])
        characterIds (:characters player)]
    (reduce (fn [game0 characterId]
              (let [items (initial-items-of game characterId)]
                (reduce (fn [game1 item] (update-player-with-item game1 playerId item))
                        game0
                        items)))
            game characterIds)))

(defn take-initial-items-for-players [game]
  (loop [n (get-in game [:conf :nbPlayers])
         g game]
    (if (zero? n)
      g
      (recur (dec n) (take-initial-items-for-player g n)))))

(defn get-first-player [game]
  (:firstPlayer game))

(defn define-first-player
  ([game]
   (let [nbPlayer (get-in game [:conf :nbPlayers])
         #^Random rnd (get-in game [:conf :random])]
     (define-first-player game
                          (inc
                            (.nextInt rnd nbPlayer)))))
  ([game playerId]
   (assoc game :firstPlayer playerId)))

(defn- ensure-identical-hands-size [hands]
  (let [rhands (vals hands)
        handSz (count (first rhands))
        allSameSz (every? #(= (count %) handSz) rhands)]
    (if-not allSameSz
      (throw (IllegalStateException. "All hands have not the same size..." hands)))
    handSz))

(defn- rotate [xs]
  (concat (rest xs) [(first xs)]))

(defn- rotate-map [kvs]
  (let [ks (keys kvs)
        nb (count ks)
        ks (cycle (sort ks))]
    (loop [cnt nb
           xs {}
           hd (first ks)
           rm (rest ks)]
      (if (zero? cnt)
        xs
        (recur
          (dec cnt)
          (assoc xs (first rm) (get kvs hd))
          (first rm)
          (rest rm))))
    ))

(defn- repopulate-hands [game missingCards]
  (let [hands (:players-hands game)
        kands (sort (keys hands))
        [newHands newDeck] (reduce
                             (fn [[nh deck] k]
                               (let [cards (take missingCards deck)
                                     ndeck (drop missingCards deck)]
                                 [(assoc nh k (concat (get hands k) cards)) ndeck]))
                             [{} (:deck game)] kands)
        newHands (rotate-map newHands)]
    (-> game
        (assoc :deck newDeck)
        (assoc :players-hands newHands))))

(defn next-round [game]
  (let [handSz (ensure-identical-hands-size (:players-hands game))
        missingCards (- 4 handSz)]
    (-> game
        (repopulate-hands missingCards)
        (update-in [:round] inc))))

