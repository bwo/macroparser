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
             :body b}))
        (many (expression))))

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
         (named :attr-map (maybe (flattened-map)))
         (arities))))

(defn defn-parser []
  (parseq->map
   (named :type (symbol 'defn))
   (named :name (symbol))
   (named :docstring (maybe (string)))
   (named :attr-map (maybe (flattened-map)))
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

(defn update-bodies
  "Update the bodies of the function definition in defn-forms by the
   function f. f will be called with, and should return, a list of forms. E.g.:

   (update-bodies '(foo [x] (println x) (inc x)) (fn [body] `((clojure.tools.logging/spy ~@body))))
    --> (defn foo [x] (lg/spy (println x) (inc x)))

   Note the return value: a list of forms, containing a single form.

   Note that wrapping function bodies (as in the above example) can interfere with \"recur\"."
  [defn-forms f]
  (let [parsed (run (parse-defn-like) defn-forms)]
    (-> (update-in parsed [:arities]
                   (partial clj/map #(update-in % [:body] f)))
        unparse-defn-like)))
