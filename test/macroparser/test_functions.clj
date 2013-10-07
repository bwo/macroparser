(ns macroparser.test-functions
  (:require [macroparser.functions :refer :all]
            [expectations :refer :all]
            [the.parsatron :as p]))

(expect '(fn foo ([x] x) ([x y] (* x y)))
        (unparse-function (p/run (parse-fn-like) '(foo ([x] x) ([x y] (* x y))))))

(expect '(defn foo "docstring" {:attr-map 1} ([x] x) ([x y] (* x y)))
        (unparse-function (p/run (parse-defn-like) '(foo "docstring" {:attr-map 1} ([x] x) ([x y] (* x y))))))

(expect '(defn foo {:attr-map 1} ([x] x) ([x y] (* x y)))
        (unparse-function (p/run (parse-defn-like) '(foo {:attr-map 1} ([x] x) ([x y] (* x y))))))
