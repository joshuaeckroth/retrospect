(ns simulator.strategies.explain)

(defn explain-recursive
  [workspace funcs]
  (loop [fs funcs
         ws workspace]
    (if (empty? fs) ws
        (let [ws2 ((first fs) ws)]
          (if ws2
            (recur fs ws2)
            (recur (rest fs) ws))))))

