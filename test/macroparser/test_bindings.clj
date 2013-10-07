(ns macroparser.test-bindings
  (:require [macroparser.bindings :refer :all]
            [expectations :refer :all]
            [the.parsatron :as p]))

(expect '[a {:keys (q w)} {x :x :as m} & {:as c}]
        (unparse-bindings (p/run (vector-binding) '[a {:keys [q w ]} {x :x :as m} & { :as c}])))

(expect '[a q w x m c]
        (bound-symbols (p/run (vector-binding) '[a {:keys [q w ]} {x :x :as m} & { :as c}])))

(expect '[a {:keys (q w), :as r} {x :x, :as m} & {:as c}]
        (unparse-bindings (p/run (vector-binding) '[a {:keys [q w] :as r} {x :x :as m} & { :as c}])))

(expect '[a q w r x m c]
        (bound-symbols (p/run (vector-binding) '[a {:keys [q w] :as r} {x :x :as m} & { :as c}])))
