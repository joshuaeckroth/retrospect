(ns simulator.types.generic)

(defprotocol Printable
  (to-str [this]))

(defprotocol Temporal
  (forward-time [this amount]))



