(ns ginkgopolis.core-spec
  (:require [speclj.core :refer :all]
            [ginkgopolis.core :refer :all]))

;         _   _ _
;   _   _| |_(_) |___
;  | | | | __| | / __|
;  | |_| | |_| | \__ \
;   \__,_|\__|_|_|___/

(defn- count-for-color [keys color]
  (->> keys
       (filter #(= color (card-color %)))
       (count)))

(defn- numbers-for-color [keys color]
  (->> keys
       (filter #(= color (card-color %)))
       (map card-number)
       (sort)))

(defn- count-for-type [keys type]
  (->> keys
       (filter #(= type (card-type %)))
       (count)))

(defn- letter-for-type [keys type]
  (->> keys
       (filter #(= type (card-type %)))
       (map card-letter)
       (sort)))

(defn- card-with-number [cards number]
  (some #(= number (card-number %)) cards))

(defn predefined-character [dst id]
  (cond (= id 1) (new-character dst :a :blue
                                [:resource]
                                (on-action :floor-construction [tile-gain])
                                1)
        (= id 2) (new-character dst :b :blue
                                [:tile]
                                (on-action :floor-construction [tile-gain])
                                1)
        (= id 3) (new-character dst :c :blue
                                [:point]
                                (on-action :exploit [tile-gain])
                                1)
        (= id 4) (new-character dst :d :yellow
                                [:resource :resource :point :tile]
                                (on-action :floor-construction [point-gain])
                                1)
        (= id 5) (new-character dst :e :red
                                [:resource :point :tile]
                                (on-action :urbanization [resource-gain])
                                1)
        :else (throw (IllegalArgumentException. (str "Invalid predefined character: " id)))))

(defn redefine-characters [game playerId characterIds]
  (-> game
      (assoc-in [:players playerId :characters] characterIds)))

;   ___ _ __   ___  ___ ___
;  / __| '_ \ / _ \/ __/ __|
;  \__ \ |_) |  __/ (__\__ \
;  |___/ .__/ \___|\___|___/
;      |_|

(describe "cartesian"
          (it "should return the cartesian product of two lists"
              (should= (for [x [-1 0 1]
                             y [-1 0 1]] (vector x y))
                       [[-1 -1] [-1 0] [-1 1]
                        [0 -1] [0 0] [0 1]
                        [+1 -1] [+1 0] [+1 1]])))

(describe "The Building Tiles"
          (it "should contain 60 tiles"
              (should= 60 (count (building-tiles))))
          (it "should contain 20 blue tiles numbered from 1 to 20"
              (should= 20 (count-for-color (keys (building-tiles)) :blue))
              (should= (range 1 21) (numbers-for-color (keys (building-tiles)) :blue)))
          (it "should contain 20 yellow tiles numbered from 1 to 20"
              (should= 20 (count-for-color (keys (building-tiles)) :yellow))
              (should= (range 1 21) (numbers-for-color (keys (building-tiles)) :yellow)))
          (it "should contain 20 red tiles numbered from 1 to 20"
              (should= 20 (count-for-color (keys (building-tiles)) :red))
              (should= (range 1 21) (numbers-for-color (keys (building-tiles)) :red))))

(describe "The Urbanization cards"
          (it "should contain 12 cards"
              (should= 12 (count (urbanization-cards))))
          (it "should be typed 'urbanization'"
              (should= 12 (count-for-type (keys (urbanization-cards)) :urbanization)))
          (it "should be lettered from 'A' to 'L'"
              (should= '(:A :B :C :D :E :F :G :H :I :J :K :L)
                       (letter-for-type (keys (urbanization-cards)) :urbanization))))

(describe "The Building cards"
          (it "should contain 60 cards"
              (should= 60 (count (building-cards))))
          (it "should all be typed 'building'"
              (should= (count (building-cards)) (count-for-type (keys (building-cards)) :building)))
          (it "should contain 20 blue cards"
              (should= 20 (count-for-color (keys (building-cards)) :blue)))
          (it "should contain 20 yellow cards"
              (should= 20 (count-for-color (keys (building-cards)) :yellow)))
          (it "should contain 20 red cards"
              (should= 20 (count-for-color (keys (building-cards)) :red))))

(describe "Building card action - red ones"
          (xit "should for card #1: gain a resource on exploit action"
               (let [card (card-with-number (building-cards) 1)
                     action (new-action card)]
                 (should-not-be-nil card))))

(describe "Character id"
          (it "should be equal when defined identically"
              (should= (->character-id 1 :blue) (->character-id 1 :blue)))
          (it "should not be equal when defined differently"
              (should-not= (->character-id 1 :blue) (->character-id 5 :blue))
              (should-not= (->character-id 1 :blue) (->character-id 1 :pink))))

(describe "The Character cards"
          (it "should contain 27 cards"
              (should= 27 (count (character-cards))))
          (it "should be typed 'character'"
              (should= 27 (count-for-type (keys (character-cards)) :character)))
          (it "should have all different ids"
              (let [ids (keys (character-cards))
                    uniques (set ids)]
                (should= 27 (count uniques)))))

(describe "Game setup"
          (it "should default to 2 players"
              (should= 2 (get-in (setup) [:conf :nbPlayers])))

          (it "should place the 9 building tiles numbered from 1, 2 and 3 for each color"
              (let [setup (setup)
                    setup-tiles-by-coord (:city setup)
                    setup-tiles (vals setup-tiles-by-coord)]
                (should= 9 (count setup-tiles-by-coord))
                (should= [1 2 3] (numbers-for-color setup-tiles :blue))
                (should= [1 2 3] (numbers-for-color setup-tiles :yellow))
                (should= [1 2 3] (numbers-for-color setup-tiles :red))))
          (it "should place the 9 building tiles in a 3x3 grid"
              (let [setup (setup)
                    setup-tiles-by-coord (:city setup)
                    coords (keys setup-tiles-by-coord)
                    grid (for [x [-1 0 1] y [-1 0 1]] (vector x y))]
                (should= (sort coords) (sort grid))))
          (it "should place *randomly* the 9 building tiles"
              (let [setup1 (setup)
                    setup2 (setup)]
                (should-not= (:city setup1) (:city setup2))))

          (it "should place the urbanization markers in alphabetical orders"
              (let [setup (setup)
                    markers (:urbanization-markers setup)]
                (should= [:a :b :c :d :e :f :g :h :i :j :k :l] (sort (keys markers)))
                (should= [-1 +2] (:a markers))
                (should= [0 +2] (:b markers))
                (should= [+1 +2] (:c markers))
                (should= [+2 +1] (:d markers))
                (should= [+2 0] (:e markers))
                (should= [+2 -1] (:f markers))
                (should= [+1 -2] (:g markers))
                (should= [0 -2] (:h markers))
                (should= [-1 -2] (:i markers))
                (should= [-2 -1] (:j markers))
                (should= [-2 0] (:k markers))
                (should= [-2 +1] (:l markers))))

          (it "should contain 45 (= 60 -9 -6) remaining tiles in the pile for a 2 or 3 players setup"
              (should= 45 (count (:tiles-general-supply (setup))))
              (should= 45 (count (:tiles-general-supply (setup {:nbPlayers 2}))))
              (should= 45 (count (:tiles-general-supply (setup {:nbPlayers 3})))))
          (it "should contain 51 (= 60 -9) remaining tiles in the pile for a 4 or 5 players setup"
              (should= 51 (count (:tiles-general-supply (setup {:nbPlayers 4}))))
              (should= 51 (count (:tiles-general-supply (setup {:nbPlayers 5})))))

          (it "should prepare the deck by shuffling together the 12 urbanization cards and the 9 building cards"
              (let [setup (setup)
                    deck (:deck setup)]
                (should= 12 (count-for-type deck :urbanization))
                (should= 9 (count-for-type deck :building))
                (should= 21 (count deck))))

          (it "should allocate, for 5 players, 16 resources per player in the general supply"
              (let [setup (setup {:nbPlayers 5})
                    resources (:resources-general-supply setup)]
                (should= [16 16 16 16 16] (vals resources))))
          (it "should allocate, for 4 players, 18 resources per player in the general supply"
              (let [setup (setup {:nbPlayers 4})
                    resources (:resources-general-supply setup)]
                (should= [18 18 18 18] (vals resources))))
          (it "should allocate, for 3 players, 20 resources per player in the general supply"
              (let [setup (setup {:nbPlayers 3})
                    resources (:resources-general-supply setup)]
                (should= [20 20 20] (vals resources))))
          (it "should allocate, for 2 players, 25 resources per player in the general supply"
              (let [setup (setup {:nbPlayers 2})
                    resources (:resources-general-supply setup)]
                (should= [25 25] (vals resources))))

          (it "should distribute 3 characters to each players - 2 players case"
              (let [setup (setup)
                    players (:players setup)]
                (should= [3 3] (map #(count (:characters %)) (vals players)))))
          (it "should distribute 3 characters to each players - 5 players case"
              (let [setup (setup {:nbPlayers 5})
                    players (:players setup)]
                (should= [3 3 3 3 3] (map #(count (:characters %)) (vals players)))))
          (it "should *randomly* distribute 3 characters to each players - 2 players case"
              (let [setup (setup)
                    players (:players setup)]
                (should-not= (get-in players [1 :characters])
                             (get-in players [2 :characters]))))
          (it "should *randomly* distribute 3 characters to each players - 5 players case"
              (let [setup (setup {:nbPlayers 5})
                    players (:players setup)
                    characterIds (reduce (fn [uniques [k v]]
                                           (apply conj uniques
                                                  (map :id (:characters v)))) #{} players)]
                (should= 15 (count characterIds))))

          (it "should not provide any tiles to any player - 5 players case"
              (let [setup (setup {:nbPlayers 5})
                    players (:players setup)]
                (should= [[] [] [] [] []] (map #(:tiles %) (vals players)))))
          (it "should not provide any resource to any player - 5 players case"
              (let [setup (setup {:nbPlayers 5})
                    players (:players setup)]
                (should= [0 0 0 0 0] (map #(:resources %) (vals players)))))
          (it "should not provide any point to any player - 5 players case"
              (let [setup (setup {:nbPlayers 5})
                    players (:players setup)]
                (should= [0 0 0 0 0] (map #(:points %) (vals players)))))

          (it "should set the round to 0"
              (should= 0 (:round (setup))))

          (it "should provide no card in players' hands"
              (let [setup (setup {:nbPlayers 5})
                    hands (:players-hands setup)]
                (should= [[] [] [] [] []] (vals hands))))
          )

(describe "Character's initial items"
          (it "should provide chracter's initial items - resource only"
              (let [game (setup)]
                (should= [:resource] (initial-items-of game (->character-id 3 :blue))))))

(describe "Draft initial items"
          (it "should increase player's resource when character allow it - resource only"
              (let [playerId 1
                    game (-> (setup {:nbPlayers 5})
                             (redefine-characters playerId [(->character-id 3 :blue)])
                             (take-initial-items-for-player playerId))
                    oplayers (vals (dissoc (:players game) playerId))]
                (should= 1 (get-in game [:players playerId :resources]))
                (should= 15 (get-in game [:resources-general-supply playerId]))
                ; everything else should remain unchanged
                (should= [0 0 0 0] (map #(:resources %) oplayers))
                (should= [0 0 0 0] (map #(:points %) oplayers))
                (should= [[] [] [] []] (map #(:tiles %) oplayers))))

          (it "should add a tile to player when character allow it - tile only"
              (let [playerId 3
                    game (-> (setup {:nbPlayers 5})
                             (redefine-characters playerId [(->character-id 21 :red)]))
                    tile1 (first (get-in game [:tiles-general-supply]))
                    game (take-initial-items-for-player game playerId)
                    oplayers (vals (dissoc (:players game) playerId))]
                (should= [tile1] (get-in game [:players playerId :tiles]))
                ; everything else should remain unchanged
                (should= [0 0 0 0] (map #(:resources %) oplayers))
                (should= [0 0 0 0] (map #(:points %) oplayers))
                (should= [[] [] [] []] (map #(:tiles %) oplayers))))

          (it "should add resources, a tile and a point to player when character allow it - multiple items"
              (let [playerId 5
                    game (-> (setup {:nbPlayers 5})
                             (redefine-characters playerId [(->character-id 5 :yellow)]))
                    tile1 (first (get-in game [:tiles-general-supply]))
                    game (take-initial-items-for-player game playerId)
                    oplayers (vals (dissoc (:players game) playerId))]
                (should= [tile1] (get-in game [:players playerId :tiles]))
                (should= 1 (get-in game [:players playerId :points]))
                (should= 2 (get-in game [:players playerId :resources]))
                (should= 14 (get-in game [:resources-general-supply playerId]))
                ; everything else should remain unchanged
                (should= [0 0 0 0] (map #(:resources %) oplayers))
                (should= [0 0 0 0] (map #(:points %) oplayers))
                (should= [[] [] [] []] (map #(:tiles %) oplayers))))

          )

(describe "Game round - Conservative checks"
          (xit "should have all initial tiles when joining all of them from city, pile and player hands")
          (xit "should have all initial resources when joining all of them from city and player hand"))

(describe "Game - select first player"
          (it "should select the provided player as first one"
              (let [setup (setup)
                    game (define-first-player setup 2)]
                (should= 2 (get-first-player game))))
          (it "should select *randomly* a player if not provided"
              (let [setup (setup {:nbPlayers 5})
                    game1 (define-first-player setup)
                    game2 (define-first-player setup)]
                (should-not= (get-first-player game1)
                             (get-first-player game2)))))

(describe "Next round - distribute cards"
          (it "should distribute 4 cards to each players for first round"
              (let [game (-> (setup {:nbPlayers 3})
                             (assoc :deck [:01 :02 :03 :04 :05 :06 :07 :08 :09 :10 :11 :12 :13 :14])
                             (define-first-player)
                             (next-round))
                    hands (vals (:players-hands game))]
                (should= 1 (:round game))
                (should= [:01 :02 :03 :04 :05 :06 :07 :08 :09 :10 :11 :12]
                         (sort (flatten hands)))))
          (it "should rotate remaining cards to next player and complete up to 4 cards"
              (let [game (-> (setup {:nbPlayers 3})
                             (assoc :deck [:10 :11 :12 :13
                                           :14 :15 :16 :17])
                             (assoc :players-hands {1 [:01 :02 :03]
                                                    2 [:04 :05 :06]
                                                    3 [:07 :08 :09]})
                             (define-first-player)
                             (next-round))
                    hands (:players-hands game)]
                (should= [:07 :08 :09 :12] (get hands 1))
                (should= [:01 :02 :03 :10] (get hands 2))
                (should= [:04 :05 :06 :11] (get hands 3))))
          )



(describe "Choose a card phase"
          (it "should wait for all player to choose a card before switching to next phase"
              (let [game (-> (setup {:nbPlayers 3})
                             (take-initial-items-for-players)
                             (define-first-player)
                             (next-round))]
                nil)))

(run-specs)