(ns retrospect.test.utils)

(defn approx=
  "Return true if the absolute value of the difference between x and y
   is less than eps."
  [x y eps]
  (< (Math/abs (- x y)) eps))

