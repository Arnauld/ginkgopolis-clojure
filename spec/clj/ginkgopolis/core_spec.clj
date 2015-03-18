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

(run-specs)