(ns simulator.types.generic)

(defprotocol Printable
  (toStr [this]))

(defprotocol Temporal
  (forwardTime [this amount]))



