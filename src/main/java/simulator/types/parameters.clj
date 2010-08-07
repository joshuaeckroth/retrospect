(ns simulator.types.parameters)

(defprotocol ParameterMethods
  (get-headers [this])
  (get-params [this])
  (to-xml [this]))
 