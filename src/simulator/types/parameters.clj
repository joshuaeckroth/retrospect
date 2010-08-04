(ns simulator.types.parameters)

(defprotocol ParameterMethods
  (getHeaders [this])
  (getParams [this])
  (toXml [this]))
 