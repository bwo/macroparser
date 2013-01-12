(ns macroparser.examples
  (:refer-clojure :exclude [map vector keyword symbol char list])
  (:use [macroparser.core]
        [the.parsatron :exclude [string]])
  (:require [clojure.core :as clj]))

;;;; parsing defns

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
                  (>>1 (many1 (list (params-and-body))) (eof))))))

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

;; a vaguely haskellish monad syntax
;; we assume that "bind" is a Clojure function with semantics like
;; Haskell's >>=.
;; let's make it possible to write this, in Clojure:
;; (mdo m1
;;      r1 <- m2
;;      r2 <- (f r1)
;;      r3 <- (bind (g r2) h)
;;      (return (inc r3)))
;; This will likely be incomprehensible without the lineation, but
;; that's ok.
;; Since we might want to perform intermediate computations without
;; doing any monadic wrapping/unwrapping, let's also enable let
;; expressions, like this:
;; (mdo m1
;;      r1 <- m2
;;      let x = (f r1)
;;          y = (g r1 x)
;;      r2 <- (h x r1)
;;      (return (i y r2)))
;; 1. In an mdo expression, we can have either:
;; (a) a regular clojure expression
;; (b) a "monadic bind" expression of the form name <- expression
;; (c) a let expression of the form let (name = expression)+
;; (in (c), parens indicate grouping, *not* syntax!)
;; the names on the LHS of a monadic bind expression or let expression
;; should be visible in all following expressions.
;; 2. an mdo expression must conclude with a regular clojure
;; expression, not a monadic bind expression or let expression.
;; We will allow destructuring in the LHS of a monadic bind
;; expression.
;; We will assume that no one is attempting to use <- as the name of a
;; variable, because that would make things incredibly confusing.
;; We have to disallow the use of "let" by itself as a name in a
;; normal clojure expression to make parsing the let expression
;; correctly simple (possible?)

(defparser monadic-bind []
  ;; nb let->> is itself an "mdo"-like form.
  (let->> [bound (binding-form-simple)
           ;; look at that blecchy underscore!
           _ (symbol '<-)
           expr (expression)]
          (always {:bound bound :expr expr :type :bind})))

(defparser let-expression []
  (lift #(do {:type :let :bindings %})
        (>> (symbol 'let)
            (many1 (attempt (let->> [bound (binding-form-simple)
                                     _ (symbol '=)
                                     expr (expression)]
                                    (always {:bound bound :expr expr})))))))

(defparser normal-expression []
  (lift (fn [expr] {:bound (gensym) :expr expr :type :normal})
        (>>1 (anything-but 'let)
             (lookahead (either (eof) (anything-but '<-))))))

(defparser parse-mdo []
  (>>1 (many (choice+ (normal-expression) (monadic-bind) (let-expression))) (eof)))

(defn unparse-m-expr [inside outside]
  (case (:type outside)
    :let `(let [~@(mapcat (fn [{:keys [bound expr]}] [(unparse-bindings bound) expr])
                          (:bindings outside))]
            ~inside)
    (:normal :bind) `(bind ~(:expr outside) (fn [~(unparse-bindings (:bound outside))]
                                             ~inside))))

(defmacro mdo [& exprs]
  (let [parsed (reverse (run (parse-mdo) exprs))]
    (assert (= :normal (:type (first parsed))) "Last expression in mdo must be a normal clojure expression.")
    (reduce unparse-m-expr (:expr (first parsed)) (rest parsed))))

;; one can now do this, which may be taking it too far.
(defparser monadic-bind' []
  (mdo bound <- (binding-form-simple)
       (symbol '<-)
       expr <- (expression)
       (always {:bound bound :expr expr :type bind})))

(defparser silly-monadic-bind []
  (mdo bound <- (binding-form-simple)
       let bound-str = (str bound)
       (if (= "x" bound-str)
         (always {:bound bound :expr bound-str :type bind})
         (mdo (symbol '<-)
              expr <- (expression)
              (always {:bound bound :expr expr :type bind})))))

;; isn't this nicer?
(defparser let-expression' []
  (lift #(do {:type :let :bindings %})
        (mdo (symbol 'let)
             (many1 (attempt (mdo bound <- (binding-form-simple)
                                  (symbol '=)
                                  expr <- (expression)
                                  (always {:bound bound :expr expr})))))))
