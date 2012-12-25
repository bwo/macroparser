(ns macroparser.examples
  (:refer-clojure :exclude [map vector keyword symbol char list])
  (:use [macroparser.core]
        [the.parsatron])
  (:require [clojure.core :as clj]))

(declare binding-form)

(defn- as-part []
  (>> (keyword :as) (symbol)))
(defn- or-part [p]
  (>> (keyword :or) p))
(defn- rest-part []
  (>> (symbol '&) (binding-form)))

(declare map-binding)

(defparser vector-binding []
  (lift #(merge {:type :vector} (zipmap [:bindings :rest :as] %))
        (parseq (many (choice (vector (vector-binding))
                              (map (map-binding))
                              (symbols-but '&)))
                (maybe (rest-part))
                (maybe (as-part)))))

(defn binding-form-simple []
  (choice (symbol) (vector (vector-binding)) (map (map-binding))))

(defparser map-binding []
  (lift #(merge {:type :map} %)
        (parseq->map
         (named :bindings
                (choice
                 (>>= (keywords :strs :syms :keys)
                      #(named % (vector (many (symbol)))))
                 (named :standard
                        (many (both (binding-form-simple)
                                    (expression))))))
         (caseparse-noconsume (keywords :or :as)
                              {:or (parseq->map (named :or (or-part (map)))
                                                (maybe (named :as (as-part))))
                               :as (parseq->map (named :as (as-part))
                                                (maybe (named :or (or-part (map)))))
                               nil (always {:or nil :as nil})}))))

(defparser binding-form []
  (choice (>> (eof) (always true))
          (binding-form-simple)))

(defparser binding-pair []
  (both (binding-form) (expression)))

(defparser binding-pairs [] (many (binding-pair)))

(defn params-and-body []
  (parseq->map (named :params (vector (vector-binding)))
               (named :body (many (expression)))))

(defn defn-parser []
  (parseq->map
   (named :type (symbol 'defn))
   (named :name (symbol))
   (named :docstring (maybe (string)))
   (named :attr-map (maybe (map)))
   (named :arities
          (either (lift clj/list (params-and-body))
                  (many1 (list (params-and-body)))))))

(declare unparse-bindings)

(defn unparse-vector-bindings [v]
  (let [bindings (clj/map unparse-bindings (:bindings v))
        as (:as v)
        rest (:rest v)]
    (vec (remove nil? (concat bindings
                              (when rest ['& rest])
                              (when as [:as as]))))))

(defn unparse-map-bindings [m]
  (let [[binding-type bindings] (first (:bindings m))
        as (:as m)
        or (:or m)
        tail (into {} (remove nil? [(when as [:as as]) (when or [:or or])]))]
    (merge tail
           (case binding-type
             (:strs :keys :syms) {binding-type bindings}
             :standard (into {} (clj/map (fn [[k v]] [(unparse-bindings k) v]) bindings))))))

(defn unparse-bindings [p]
  (if (symbol? p) p
      (case (:type p)
        :vector (unparse-vector-bindings p)
        :map (unparse-map-bindings p))))

(defn unparse-arities [arities]
  (if (= 1 (count arities))
    (list* (unparse-bindings (:params (first arities)))
           (:body (first arities)))
    (clj/map (fn [a] (list* (unparse-bindings (:params a))
                           (:body a))) arities)))

(defn unparse-defn [m]
  (remove nil?
          (list* (:type m)
                 (:name m)
                 (:docstring m)
                 (:attr-map m)
                 (unparse-arities (:arities m)))))

