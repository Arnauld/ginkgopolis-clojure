(ns ginkgopolis.core)

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
       '(
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
          {:letter :L})))