(ns macroparser.functions
  (:require [macroparser.bindings :as bindings])
  (:refer-clojure :exclude [vector map symbol])
  (:use [the.parsatron :exclude [string]])
  (:use [macroparser.parsers]))


(defn params-and-body []
  (parseq->map (named :params (vector (bindings/vector-binding)))
               (named :body (many (expression)))))

(defn defn-parser []
  (parseq->map
   (named :type (symbol 'defn))
   (named :name (symbol))
   (named :docstring (maybe (string)))
   (named :attr-map (maybe (map)))
   (named :arities
          (either (lift clj/list (params-and-body))
                  (>>1 (many1 (list (params-and-body))) (eof))))))

(defn unparse-arities [arities]
  (if (= 1 (count arities))
    (list* (bindings/unparse-bindings (:params (first arities)))
           (:body (first arities)))
    (clj/map (fn [a] (list* (bindings/unparse-bindings (:params a))
                           (:body a))) arities)))

(defn unparse-defn [m]
  (remove nil?
          (list* (:type m)
                 (:name m)
                 (:docstring m)
                 (:attr-map m)
                 (unparse-arities (:arities m)))))

