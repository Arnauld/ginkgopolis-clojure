(ns ginkgopolis.core-spec
  (:require [speclj.core :refer :all]
            [ginkgopolis.core :refer :all]))

;         _   _ _
;   _   _| |_(_) |___
;  | | | | __| | / __|
;  | |_| | |_| | \__ \
;   \__,_|\__|_|_|___/

(defn- count-for-color [tiles color]
  (->> tiles
       (filter #(= color (:color %)))
       (count)))

(defn- numbers-for-color [tiles color]
  (->> tiles
       (filter #(= color (:color %)))
       (map :number)
       (sort)))

(defn- count-for-type [tiles type]
  (->> tiles
       (filter #(= type (:card-type %)))
       (count)))

(defn- letter-for-type [tiles type]
  (->> tiles
       (filter #(= type (:card-type %)))
       (map :letter)
       (sort)))

(defn- card-with-number [cards number]
  (some #(= number (:number %)) cards))

(defn predefined-character [id]
  (cond (= id 1) (new-character :a :blue
                                [:resource]
                                (on-action :floor-construction [tile-gain])
                                1)
        (= id 2) (new-character :b :blue
                                [:tile]
                                (on-action :floor-construction [tile-gain])
                                1)
        (= id 3) (new-character :c :blue
                                [:point]
                                (on-action :exploit [tile-gain])
                                1)
        (= id 4) (new-character :d :yellow
                                [:resource :resource :point :tile]
                                (on-action :floor-construction [point-gain])
                                1)
        (= id 5) (new-character :e :red
                                [:resource :point :tile]
                                (on-action :urbanization [resource-gain])
                                1)
        :else (throw (IllegalArgumentException. (str "Invalid predefined character: " id)))))

(defn redefine-characters [game playerId characters]
  (assoc-in game [:players playerId :characters] characters))

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
              (should= 20 (count-for-color (building-tiles) :blue))
              (should= (range 1 21) (numbers-for-color (building-tiles) :blue)))
          (it "should contain 20 yellow tiles numbered from 1 to 20"
              (should= 20 (count-for-color (building-tiles) :yellow))
              (should= (range 1 21) (numbers-for-color (building-tiles) :yellow)))
          (it "should contain 20 red tiles numbered from 1 to 20"
              (should= 20 (count-for-color (building-tiles) :red))
              (should= (range 1 21) (numbers-for-color (building-tiles) :red))))

(describe "The Urbanization cards"
          (it "should contain 12 cards"
              (should= 12 (count (urbanization-cards))))
          (it "should be typed 'urbanization'"
              (should= 12 (count-for-type (urbanization-cards) :urbanization)))
          (it "should be lettered from 'A' to 'L'"
              (should= '(:A :B :C :D :E :F :G :H :I :J :K :L)
                       (letter-for-type (urbanization-cards) :urbanization))))

(describe "The Building cards"
          (it "should contain 60 cards"
              (should= 60 (count (building-cards))))
          (it "should all be typed 'building'"
              (should= (count (building-cards)) (count-for-type (building-cards) :building)))
          (it "should contain 20 blue cards"
              (should= 20 (count-for-color (building-cards) :blue)))
          (it "should contain 20 yellow cards"
              (should= 20 (count-for-color (building-cards) :yellow)))
          (it "should contain 20 red cards"
              (should= 20 (count-for-color (building-cards) :red))))

(describe "Building card action - red ones"
          (xit "should for card #1: gain a resource on exploit action"
               (let [card (card-with-number (building-cards) 1)
                     action (new-action card)]
                 (should-not-be-nil card))))

(describe "The Character cards"
          (it "should contain 27 cards"
              (should= 27 (count (character-cards))))
          (it "should be typed 'character'"
              (should= 27 (count-for-type (character-cards) :character)))
          (it "should have all different ids"
              (let [ids (map #(:id %) (character-cards))
                    uniques (set ids)]
                (should= 27 (count uniques)))))

(describe "Game setup"
          (it "should default to 2 players"
              (should= 2 (:nbPlayers (setup))))

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
          )

(describe "Draft initial items"
          (it "should increase player's resource when character allow it - resource only"
              (let [playerId 1
                    setup (setup {:nbPlayers 5})
                    setup (redefine-characters setup playerId [(predefined-character 1)])
                    update (take-initial-items-for-player setup playerId)
                    players (:players update)
                    oplayers (vals (dissoc players playerId))]
                (should= 1 (get-in update [:players playerId :resources]))
                (should= 15 (get-in update [:resources-general-supply playerId]))
                ; everything else should remain unchanged
                (should= [0 0 0 0] (map #(:resources %) oplayers))
                (should= [0 0 0 0] (map #(:points %) oplayers))
                (should= [[] [] [] []] (map #(:tiles %) oplayers))))

          (it "should add a tile to player when character allow it - tile only"
              (let [playerId 3
                    setup (setup {:nbPlayers 5})
                    setup (redefine-characters setup playerId [(predefined-character 2)])
                    tile1 (first (get-in setup [:tiles-general-supply]))
                    update (take-initial-items-for-player setup playerId)
                    players (:players update)
                    oplayers (vals (dissoc players playerId))]
                (should= [tile1] (get-in update [:players playerId :tiles]))
                ; everything else should remain unchanged
                (should= [0 0 0 0] (map #(:resources %) oplayers))
                (should= [0 0 0 0] (map #(:points %) oplayers))
                (should= [[] [] [] []] (map #(:tiles %) oplayers))))

          (it "should add a point to player when character allow it - point only"
              (let [playerId 4
                    setup (setup {:nbPlayers 5})
                    setup (redefine-characters setup playerId [(predefined-character 3)])
                    update (take-initial-items-for-player setup playerId)
                    players (:players update)
                    oplayers (vals (dissoc players playerId))]
                (should= 1 (get-in update [:players playerId :points]))
                ; everything else should remain unchanged
                (should= [0 0 0 0] (map #(:resources %) oplayers))
                (should= [0 0 0 0] (map #(:points %) oplayers))
                (should= [[] [] [] []] (map #(:tiles %) oplayers))))

          (it "should add resources, a tile and a point to player when character allow it - multiple items"
              (let [playerId 5
                    setup (setup {:nbPlayers 5})
                    setup (redefine-characters setup playerId [(predefined-character 4)])
                    tile1 (first (get-in setup [:tiles-general-supply]))
                    update (take-initial-items-for-player setup playerId)
                    players (:players update)
                    oplayers (vals (dissoc players playerId))]
                (should= [tile1] (get-in update [:players playerId :tiles]))
                (should= 1 (get-in update [:players playerId :points]))
                (should= 2 (get-in update [:players playerId :resources]))
                (should= 14 (get-in update [:resources-general-supply playerId]))
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

(run-specs)