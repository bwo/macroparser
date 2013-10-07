(ns macroparser.functions
  (:require [macroparser.bindings :as bindings]
            [clojure.core :as clj])
  (:refer-clojure :exclude [vector map symbol char keyword list seq])
  (:use [the.parsatron :exclude [string]])
  (:use [macroparser.parsers]))

(defn body []
  (lift (fn [b]
          (if (and (> (count b) 1) (map? (first b)))
            {:prepost (first b)
             :body (rest b)}
            {:prepost nil
             :body b})) (many (expression))))

(defn params-and-body []
  (let->> [params (vector (bindings/vector-binding))
           prepost-body (body)]
          (always (merge {:params params} prepost-body))))

(defparser arities []
  (named :arities
         (either (lift clj/list (params-and-body))
                 (>>1 (many1 (list (params-and-body))) (eof)))))

(defparser parse-defn-like []
  (lift (fn [res] (assoc res :type 'defn))
        (parseq->map
         (named :name (symbol))
         (named :docstring (maybe (string)))
         (named :attr-map (maybe (map)))
         (arities))))

(defn defn-parser []
  (parseq->map
   (named :type (symbol 'defn))
   (named :name (symbol))
   (named :docstring (maybe (string)))
   (named :attr-map (maybe (map)))
   (arities)))

(defparser parse-fn-like []
  (lift (fn [res] (assoc res :type 'fn))
        (parseq->map
         (named :name (maybe (symbol)))
         (arities))))

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

(defn unparse-arity [a prepost body]
  (let [f (if (seq? body) clj/list* clj/list)]
    (if prepost
      (f (bindings/unparse-bindings a) prepost body)
      (f (bindings/unparse-bindings a) body))))

(defn unparse-arities [arities]
  (if (= 1 (count arities))
    (unparse-arity (:params (first arities)) (:prepost (first arities)) (:body (first arities)))
    (clj/map (fn [a] (unparse-arity (:params a) (:prepost a) (:body a))) arities)))

(defn unparse-defn-like [m]
  (remove nil?
          (list* (:type m)
                 (:name m)
                 (:docstring m)
                 (:attr-map m)
                 (unparse-arities (:arities m)))))

(defn unparse-fn-like [m]
  (remove nil?
          (list* (:type m)
                 (:name m)
                 (unparse-arities (:arities m)))))

(defn unparse-function [m]
  ((case (:type m)
     (fn fn*) unparse-fn-like
     defn unparse-defn-like) m))
