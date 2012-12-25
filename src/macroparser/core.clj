(ns macroparser.core
  (:refer-clojure :exclude [symbol vector keyword char map list])
  (:use the.parsatron)
  (:import [the.parsatron Ok Err InputState LineColPos Continue])
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
  (fn [got pos] (error-at pos (str "expected " tname ", got " (ckeof got (fn [_] (str (type got) ": " got)))))))
(defn expect-specific [thing]
  (fn [got pos] (error-at pos (str "expected " thing ", got " (ckeof got identity)))))
(defn expect-several [things]
  (fn [got pos] (error-at pos (str "expected one of " things ", got " (ckeof got identity)))))

(defn token-err [f err]
  (token-err-by (fn [tok] (if (f tok) tok nil)) err))

(defn lift
  "(lift f p) -> (bind p (comp always f)"
  [f p] (bind p (comp always f)))

(defn both
  "Parse p and then q, returning the results of both in order."
  [p q]
  (bind p (fn [pres] (lift (fn [qres] [pres qres]) q))))

(defn named [name parser]
  (lift (fn [res] {name res}) parser))

(defmacro parseq
  "Like >> for nxt. Expands into repeated both forms, flattened. (It will *parse* a *seq*uence.)"
  ([p] p)
  ([p q] `(both ~p ~q))
  ([p q & rs] `(lift (fn [[x# rest#]] (concat [x#] rest#))
                     (both ~p (parseq ~q ~@rs)))))

(defn ->map
  "Construct a single map from a sequence of named parsers."
  [p]  
  (lift (fn [m] (if (map? m) m (apply merge m))) p))

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
                         Err (eerr# (:errors result#))))
                     (eerr# (on-err# tok# pos#)))
                   (eerr# (on-err# ::eof pos#)))))))))

(make-container-parser vector identity)
(make-container-parser list identity)
(make-container-parser map (comp flatten-1 seq))

(defn caseparse-noconsume
  "Run p, wrapped in a maybe, without consuming input, and run one of
  the parsers in the cases map depending on p's output."
  [p cases]
  (bind (lookahead (maybe p)) #(get cases % (never))))

(defn caseparse
  "Run p, wrapped in a maybe, consuming input, and run one of the
  parsers in the cases map depending on p's result."
  [p cases]
  (bind (maybe p) #(get cases % (never))))

;; example!

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
        (->map
         (parseq
          (named :bindings
                 (choice
                  (bind (keywords :strs :syms :keys)
                        #(named % (vector (many (symbol)))))
                  (named :standard
                         (many (both (binding-form-simple)
                                     (expression))))))
          (caseparse-noconsume (keywords :or :as)
                               {:or (->map (both (named :or (or-part (map)))
                                                 (maybe (named :as (as-part)))))
                                :as (->map (both (named :as (as-part))
                                                 (maybe (named :or (or-part (map))))))
                                nil (always {:or nil :as nil})})))))

(defparser binding-form []
  (choice (>> (eof) (always true))
          (binding-form-simple)))

(defparser binding-pair []
  (both (binding-form) (expression)))

(defparser binding-pairs [] (many (binding-pair)))

(defn params-and-body []
  (->map (both (named :params (vector (vector-binding)))
               (named :body (many (expression))))))

(defn defn-parser []
  (->map
   (parseq
    (named :defn (symbol 'defn))
    (named :name (symbol))
    (named :docstring (maybe (string)))
    (named :attr-map (maybe (map)))
    (named :arities
           (either (lift clj/list (params-and-body))
            (many1 (list (params-and-body))))))))
