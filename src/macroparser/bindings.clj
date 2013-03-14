(ns macroparser.bindings
  (:refer-clojure :exclude [map vector keyword symbol char list seq])
  (:require [clojure.core :as clj])
  (:use [macroparser.parsers]
        [the.parsatron :exclude [string]]))

(declare binding-form)

(defn- as-part []
  (>> (keyword :as) (symbol)))
(defn- or-part [p]
  (>> (keyword :or) p))
(defn- rest-part []
  (>> (symbol '&) (binding-form)))

(declare map-binding)
(declare binding-form-simple)

(defparser vector-binding []
  (lift #(merge {:type :vector} (zipmap [:bindings :rest :as] %))
        (parseq (many (binding-form-simple '&))
                (maybe (rest-part))
                (maybe (as-part)))))

(defn binding-form-simple [& exclude-symbols]
  (choice (apply symbols-but exclude-symbols) (vector (vector-binding)) (map (map-binding))))

(defparser map-binding []
  (lift #(merge {:type :map} %)
        (parseq->map
         (named :bindings
                (choice
                 (bind (keywords :strs :syms :keys)
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

(defn bound-symbols [bindings]
  (cond
   (symbol? bindings)  [bindings]
   (seq? bindings) (clj/map bound-symbols bindings)
   (= :vector (:type bindings)) (flatten (clj/mapcat #(when-let [b (% bindings)]  (bound-symbols b)) [:bindings :as :rest]))
   (= :map (:type bindings)) (let [[binding-type inner] (first (:bindings bindings))]
                               (case binding-type
                                 (:str :keys :syms)  inner
                                 :standard (concat (mapcat bound-symbols (clj/map first inner)) (:as bindings))))))

(declare unparse-bindings)

(defn unparse-vector-bindings [v]
  (let [bindings (clj/map unparse-bindings (:bindings v))
        as (:as v)
        rest (:rest v)]
    (vec (remove nil? (concat bindings
                              (when rest ['& (unparse-bindings rest)])
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
