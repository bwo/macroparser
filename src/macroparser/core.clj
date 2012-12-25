(ns macroparser.core
  (:refer-clojure :exclude [symbol vector keyword char map list])
  (:use the.parsatron)
  (:import [the.parsatron Ok Err InputState LineColPos Continue ParseError])
  (:require [clojure.core :as clj]))

;; some extra general functions

;; match and transform a token
(defn token-by
  [f]
  (fn [{:keys [input pos] :as state} cok cerr eok eerr]
    (if-let [tok (first input)]
      (if-let [res (f tok)]
        (cok res (InputState. (rest input) (increment-position pos tok)))
        (eerr (unexpect-error (str "token '" tok "'") pos)))
      (eerr (unexpect-error "end of input" pos)))))

(defn token-err-by
  [f err]
  (fn [{:keys [input pos] :as state} cok cerr eok eerr]
    (if-let [tok (first input)]
      (if-let [res (f tok)]
        (cok res (InputState. (rest input) (increment-position pos tok)))
        (eerr (err tok pos)))
      (eerr (err ::eof pos)))))

(defn ckeof [got f] (if (= got ::eof) "end of input" (f got)))

(defn run-inferior [p input nesting]
  (let [state (InputState. input (LineColPos. nesting 1))]
    (run-parser p state)))

(defn expect-type [tname]
  (fn [got pos] (ParseError. pos [(str "expected " tname ", got " (ckeof got (fn [_] (str (type got) ": " got))))])))
(defn expect-specific [thing]
  (fn [got pos] (ParseError. pos [(str "expected " thing ", got " (ckeof got identity))])))
(defn expect-several [things]
  (fn [got pos] (ParseError. pos [(str "expected one of " things ", got " (ckeof got identity))])))

(defn token-err [f err]
  (token-err-by (fn [tok] (if (f tok) tok nil)) err))

(defmacro >>=
  "(>>= p f) -> (bind p (comp always f)"
  [p f] `(bind ~p (comp always ~f)))

(defn both
  "Parse p and then q, returning the results of both in order."
  [p q]
  (bind p (fn [pres] (>>= q (fn [qres] [pres qres])))))

(defn named [name parser]
  (>>= parser (fn [res] {name res})))

(defn optional
  "Attempt to parse p, and on failure proceed without consuming input."
  [p]
  (>>= (choice (times 1 p) (times 0 p)) first))

(defmacro parseq
  "Like >> for nxt. Expands into repeated both forms, flattened. (It will *parse* a *seq*uence.)"
  ([p] p)
  ([p q] `(both ~p ~q))
  ([p q & rs] `(>>= (both ~p (parseq ~q ~@rs)) (fn [[x# rest#]] (concat [x#] rest#)))))

(defn ->map
  "Construct a single map from a sequence of named parsers."
  [p]  
  (>>= p (fn [m] (if (map? m) m (apply merge m)))))

(defmacro ^{:private true} make-type-matcher [name]
  (let [s (str name)
        plural (str s "s")
        pluralsym (clj/symbol plural)
        test (clj/symbol (str s "?"))]
    `(do
       (defn ~name ~(str "match any " s " (with no args) or a specific " s)
         ([] (token-err ~test (expect-type ~s)))
         ([tok#] (token-err #(and (~test %) (= tok# %)) (expect-specific tok#))))
       (defn ~pluralsym ~(str "match any of several " plural)
         [& toks#]
         (token-err
          (fn [inp#] (some #(= inp# %) toks#)) (expect-several toks#))))))

(make-type-matcher symbol)

(defn- symbols-but
  "match any symbol but those provided as arguments"
  [& syms]
  (token (fn [inp] (and (symbol? inp) (not-any? #(= inp %) syms)))))

(make-type-matcher keyword)

(defparser maybe [p]
  (either p (always nil)))

(defn string [] (token-err string? (expect-type "string")))
(defn integer [] (token-err integer? (expect-type "integer")))
(defn expression [] (token (fn [_] true)))

(defn flatten-1 [xs]
  (lazy-seq
   (when-let [x (first (seq xs))]
     (if (sequential? x)
       (concat x (flatten-1 (rest xs)))
       (cons x (flatten-1 (rest xs)))))))

(defmacro ^{:private true} make-container-parser [name preprocessor]
  (let [s (str name)
        test (clj/symbol (str s "?"))]
    `(defn ~name
       ~(str "match a " s " and run any provided parser on its contents")
       ([] (token-err ~test (expect-type ~s)))
       ([p#] (fn [state# cok# _# _# eerr#]
               (let [on-err# (expect-type ~s)
                     input# (:input state#)
                     pos# (:pos state#)]
                 (println input#)
                 (if-let [tok# (first input#)]
                   (if (~test tok#)
                     (let [result# (run-inferior
                                    (let->> [r# p#
                                             _# (eof)]
                                            (always r#))
                                    (~preprocessor tok#)
                                    (inc (:line pos#)))]                       
                       (condp instance? result#
                         Ok (cok#
                             (:item result#)
                             (InputState. (rest input#)
                                          (increment-position pos# tok#)))
                         Err (eerr# (:errmsg result#))))
                     (eerr# (on-err# tok# pos#)))
                   (eerr# (on-err# ::eof pos#)))))))))

(extend-type java.lang.String
  ShowableError
  (show-error [s] s))

(make-container-parser vector identity)
(make-container-parser list identity)
(make-container-parser map (comp flatten-1 seq))

;; example!

(declare binding-form)

(defn- as-part []
  (both (keyword :as) (symbol)))
(defn- or-part [p]
  (both (keyword :or) p))
(defn- rest-part []
  (>> (symbol '&) (binding-form)))

(declare map-binding)

(defparser vector-binding []
  (>>= (parseq (many (choice (vector (vector-binding))
                             (map (map-binding))
                             (symbols-but '&)))
               (maybe (rest-part))
               (>>= (maybe (as-part)) second))
       #(zipmap [:bindings :rest :as] %)))

(defparser map-binding []
  (let->>
   [bindings (choice (both (keywords :strs :syms :keys) (vector (many1 (symbol))))
                     (many1 (both (choice
                                   (symbol)
                                   (vector (vector-binding))
                                   (map (map-binding)))
                                  (expression))))
    mod-parts (times 2 (maybe (either (or-part (map))
                                      (as-part))))]
   (let [mod-parts (apply merge (clj/map (fn [v] (when v {(first v) (second v)})) mod-parts))]
     (always {:bindings bindings :mod-parts mod-parts}))))

(defn map->binding [{:keys [bindings mod-parts rest as] :or {bindings [] mod-parts nil rest nil as nil}}]
  (let [bindings (clj/map #(if (map? %) (map->binding %) %) bindings)
        mod-parts (merge {:as as} mod-parts)]
    (vec (concat bindings
                 (when rest ['& rest])
                 (when-let [or (:or mod-parts)] [:or or])
                 (when-let [as (:as mod-parts)] [:as as])))))

(defparser binding-form []
  (choice (>> (eof) (always true))
          (symbol)
          (vector (vector-binding))
          (map (map-binding))))

(defparser binding-pair []
  (both (binding-form) (expression)))

(defparser binding-pairs [] (many (binding-pair)))

(defn op [] (symbols '= '< '>))
(defn field-part [] (list (both
                           (keyword)
                           (many1 (list (both (op) (expression)))))))
(defn fn-part [] (list (parseq
                       (symbol 'fn)
                       (symbol)
                       (expression))))
(defn atom-parser [] (either (field-part) (fn-part)))
(defn query-parser [] (choice
                     (atom-parser)
                     (list (both (symbols 'and 'or)
                                 (many1 (atom-parser))))))

(defn defn-parser []
  (->map
   (parseq
    (named :defn (symbol 'defn))
    (named :name (symbol))
    (named :docstring (optional (string)))
    (named :attr-map (optional (map)))
    (either
     (named :arities (->map (both (named :params (binding-form))
                                  (named :body (many (expression))))))
     (named :arities (many1 (list (->map (both (named :params (binding-form))
                                               (named :body (many (expression))))))))))))
