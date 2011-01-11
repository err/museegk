(ns museegk.utils
  (:require [clojure.contrib.combinatorics :as combo]))




(defn n-iterate
  "Returns a (length n) sequence of x, (f x), (f (f x)), etc"
  [n f x]
  (take n (iterate f x)))


(def matrix-coords [r-coords c-coords]
     (clojure.contrib.combinatorics/*classpath*))

