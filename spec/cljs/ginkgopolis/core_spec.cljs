(ns ginkgopolis.core-spec 
  (:require-macros [speclj.core :refer [describe it should=]])
  (:require [speclj.core]
            [ginkgopolis.core]))

(describe "A ClojureScript test"
  (it "fails. Fix it!"
    (should= 0 1)))
