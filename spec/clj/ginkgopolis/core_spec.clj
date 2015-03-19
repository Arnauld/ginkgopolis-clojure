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
          (xit "should contain 60 cards"
               (should= 60 (count (building-cards))))
          (it "should all be typed 'building'"
              (should= (count (building-cards)) (count-for-type (building-cards) :building)))
          (it "should contain 20 red cards"
              (should= 20 (count-for-color (building-cards) :red))))

(describe "Building card action - red ones"
          (xit "should for card #1: gain a resource on exploit action"
               (let [card (card-with-number (building-cards) 1)
                     action (new-action card)]
                 (should-not-be-nil card))))


(describe "Game setup"
          (it "should place the 9 building tiles numbered from 1, 2 and 3 for each color"
              (let [setup (setup)
                    setup-tiles-by-coord (:tiles setup)
                    setup-tiles (vals setup-tiles-by-coord)]
                (should= 9 (count setup-tiles-by-coord))
                (should= [1 2 3] (numbers-for-color setup-tiles :blue))
                (should= [1 2 3] (numbers-for-color setup-tiles :yellow))
                (should= [1 2 3] (numbers-for-color setup-tiles :red))))
          (it "should place the 9 building tiles in a 3x3 grid"
              (let [setup (setup)
                    setup-tiles-by-coord (:tiles setup)
                    coords (keys setup-tiles-by-coord)
                    grid (for [x [-1 0 1] y [-1 0 1]] (vector x y))]
                (should= (sort coords) (sort grid))))
          (it "should place *randomly* the 9 building tiles"
              (let [setup1 (setup)
                    setup2 (setup)]
                (should-not= (:tiles setup1) (:tiles setup2)))))

(run-specs)