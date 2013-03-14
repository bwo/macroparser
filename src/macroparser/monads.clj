(ns macroparser.monads
  (:refer-clojure :exclude [symbol char map vector list keyword seq])
  (:use [the.parsatron :exclude [string]]
        [macroparser.parsers])
  (:require [macroparser.bindings :as bindings]))

;; The problem: domonad is ugly.
;; The solution: a nicer monad syntax. In fact, why not copy Haskell's
;; do notation? (More or less---the reader means that we can't use
;; newlines, e.g.)
;;
;; The hideousness of domonad, in brief:
;;
;; 1. It expresses "monad comprehensions" in the form of a binding
;;    vector followed by an expression, similar to "let" (except
;;    without the implicit do, which isn't a big deal). Using a
;;    binding vector might make sense in cases where every expression
;;    produces a value that interests us, but that is manifestly not
;;    the case for monadic computations, which are frequently employed
;;    for their "side effects". The vector must then be littered with
;;    underscores.
;;
;; 2. domonad wraps the result of the final expression (i.e. the one
;;    that follows the binding vector) in m-return, which means that
;;    you have to jump through hoops if the final expression already
;;    returns a monadic value: you have to do
;;
;;      (domonad [a (whatever)
;;                b (whatever-2 a)]
;;               b)
;;
;;    rather than the more sensible
;;
;;      (domonad [a (whatever)]
;;               (whatever-2 a))
;;
;;    The first option doesn't actually introduce an extra monadic
;;    bind, but only because domonad optimizes it away when the final
;;    expression is identical to the final bound name. But it's still
;;    visually obtuse and the optimization isn't necessary in the
;;    first place.
;;
;; 3. Binding vectors aren't very expressive. But frequently we want
;;    to do various things in the course of a monadic chain of
;;    computations: compute intermediate results using pure functions,
;;    branch in various ways, only proceed if some condition is met,
;;    etc. Now in fact we don't need to do anything special to support
;;    this, as the following will work (or would if it weren't for the
;;    double m-return that would result):
;;
;;      (domonad [a (whatever)]
;;               (cond
;;                (= a :x) (domonad [...] ...)
;;                (even? a) (domonad [...] ...)
;;                :otherwise (let [b ...] (domonad [...] ...))))
;;
;;    but if things get complicated we'll be very indented, even if we
;;    instruct our editors about how to indent "domonad". And the
;;    whole thing threatens to become cumbersome. (If not factored!)
;;
;;    Domonad's response to this is to grow a bunch of optional
;;    keywords that can follow the expression part of a binding pair:
;;    :if/:then/:else, :let, :when, and :cond, at the moment. These
;;    ungainly growths affect the evaluation (or evaluation
;;    environment) of the bindings that follow, even though they do
;;    not syntactically contain them, and represent, to my mind, a
;;    half-hearted commitment to the idea of a DSL:
;;
;;      (domonad [a (whatever) :let [b (+ a 1)]
;;                c (whatever2 a b) :when (even? a)
;;                _ (foo)
;;                :if (> c 3) :then [d (whatever2 c)] :else [d (whatever2 (/ c 2))]
;;                ;; I don't even want to think about what the cond form looks
;;                ;; like here.
;;                ]
;;               (+ d 1))
;;
;;    Totes grody!
;;
;; 4. "binding vector followed by expression in the environment
;;    established by the bindings" is a bad model for sequences of
;;    monadic computations. The business of the vector isn't just
;;    establishing bindings, followed by the real doing-things action
;;    of the final expression; they *all* do things; occasionally we
;;    also want to grab a value produced and give it a name. It would
;;    be nice if our syntax reflected this parity.
;;
;; Our alternative will allow the above to be expressed thus:
;;
;;    (mdo a <- whatever
;;         let b = (+ a 1)
;;         c <- (whatever2 a b)
;;         (guard (even? a))
;;         (foo)
;;         d <- (if (> c 3) (whatever2 c) (whatever2 (/ c 2)))
;;         (m-return (+ d 1)))
;;
;; assuming an appropriate definition of "guard" (which would be
;; equivalent to Haskell's "when"). (Assuming also that :when in
;; domonad affects *following* bindings---I'm not actually certain of
;; that.) Note that in this case "guard" is just an ordinary (albeit
;; monadic) Clojure function, and in principle anything could have
;; taken its place: we didn't need to fancy up our macro to implement
;; it. Note also that "mdo" requires an explicit m-return if the last
;; expression doesn't already return a monadic value, and that monadic
;; computations being used for their "side effects" are cleanly
;; integrated.
;;
;; We could also have done without fancying up our macro to allow let
;; bindings the way we did, in which case we would have written the
;; above like this:
;;
;;    (mdo a <- whatever
;;         (let [b (+ a 1)]
;;           (mdo c <- whatever2 a b)
;;           ...))
;;
;; And it's true that it contravenes normal lispy practice to have the
;; binding b established in code it does not lexically enclose
;; (outside of special contexts such as "let"). But this macro wears
;; on its sleeve the fact that it is outside normal lispy syntax, so I
;; think it's ok.
;;
;; Given the tools we already have---in particular, a parser for
;; binding forms---implementing this is dead simple.

;; Since we have no dependency on algo.monads, "mdo" below actually
;; only works for parsers. This will be remedied no later than the
;; point at which the parsatron makes itself a respectable monad
;; according to algo.monads' lights.

(defparser monadic-bind []
  ;; nb let->> is itself an "mdo"-like form.
  (let->> [bound (bindings/binding-form-simple)
           ;; look at that blecchy underscore!
           _ (symbol '<-)
           expr (expression)]
          (always {:bound bound :expr expr :type :bind})))

(defparser let-expression []
  (lift #(do {:type :let :bindings %})
        (>> (symbol 'let)
            (many1 (attempt (let->> [bound (bindings/binding-form-simple)
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
    :let `(let [~@(mapcat (fn [{:keys [bound expr]}] [(bindings/unparse-bindings bound) expr])
                          (:bindings outside))]
            ~inside)
    (:normal :bind) `(bind ~(:expr outside) (fn [~(bindings/unparse-bindings (:bound outside))]
                                             ~inside))))

(defmacro mdo [& exprs]
  (let [parsed (reverse (run (parse-mdo) exprs))]
    (assert (= :normal (:type (first parsed))) "Last expression in mdo must be a normal clojure expression.")
    (reduce unparse-m-expr (:expr (first parsed)) (rest parsed))))
