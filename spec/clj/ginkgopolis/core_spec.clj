(ns ginkgopolis.core-spec
  (:require [speclj.core :refer :all]
            [ginkgopolis.core :refer :all]))

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

(describe "Character card"
          (it "should contain 27 cards"
              (should= 27 (count (character-cards))))
          (it "should be typed 'character'"
              (should= 27 (count-for-type (character-cards) :character))))

(describe "Game setup"
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
          (it "should contain 51 (= 60 -9) remaining tiles in the pile for a 4 players setup"
              (should= 51 (count (:tiles-general-supply (setup {:nbPlayers 4})))))

          (it "should prepare the deck by shuffling together the 12 urbanization cards and the 9 building cards"
              (let [setup (setup)
                    deck (:deck setup)]
                (should= 12 (count-for-type deck :urbanization))
                (should= 9 (count-for-type deck :building))
                (should= 21 (count deck))))
          )

(run-specs)