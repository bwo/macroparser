(ns macroparser.functions
  (:require [macroparser.bindings :as bindings]
            [clojure.core :as clj])
  (:refer-clojure :exclude [vector map symbol char keyword list])
  (:use [the.parsatron :exclude [string]])
  (:use [macroparser.parsers]))


(defn params-and-body []
  (parseq->map (named :params (vector (bindings/vector-binding)))
               (named :body (many (expression)))))

(defparser arities []
  (named :arities
         (either (lift clj/list (params-and-body))
                 (>>1 (many1 (list (params-and-body))) (eof)))))

(defn defn-parser []
  (parseq->map
   (named :type (symbol 'defn))
   (named :name (symbol))
   (named :docstring (maybe (string)))
   (named :attr-map (maybe (map)))
   (arities)))

(defn fn-parser []
  (parseq->map
   (named :type (symbols 'fn* 'fn))
   (named :name (maybe (symbol)))
   (arities)))

(defn function-parser []
  (caseparse-noconsume (symbols 'fn 'defn 'fn*)
                       {'fn (fn-parser)
                        'fn* (fn-parser)
                        'defn (defn-parser)}))

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

(defn unparse-fn [m]
  (remove nil?
          (list* (:type m)
                 (:name m)
                 (unparse-arities (:arities m)))))

(defn unparse-function [m]
  ((case (:type m)
     (fn fn*) unparse-fn
     defn unparse-defn) m))
