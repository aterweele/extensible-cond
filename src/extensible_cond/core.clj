(ns extensible-cond.core
  (:refer-clojure :exclude [cond]))

(defmulti clause
  "Build a clause for `cond` for dispatch value `k`"
  (fn [k _ _] k)
  :default ::default
  ;; TODO consider alternate hierarchy
  )

(defmacro cond
  "clause => lhs rhs

  For each lhs, determine how to evaluate rhs, whether to process the
  rest of the clauses, etc."
  ;; Note that this docstring is augmented by invocations of
  ;; `defclause`.
  [& clauses]
  ;; TODO either handle implicit else or enforce even number of forms.
  (->> clauses
       (partition 2)
       reverse
       (reduce (fn [child [k rhs]] (clause k rhs child)) nil)))

(defmacro defclause
  [dispatch docstring [lhs rhs child] & body]
  (alter-meta! #'cond
               (fn [{:keys [doc] :as meta}]
                 (assoc meta :doc (format "%s\n\n%s - %s" doc dispatch docstring))))
  `(defmethod clause ~dispatch [~lhs ~rhs ~child] ~@body))

;; TODO or should it be that non-keywords have this behavior? I think
;; no, because if people want arbitrary behavior based on the type or
;; value of lhs, then who am I to deny them? OTOH, pinning down the
;; non-keyword behavior would limit the possibility for different
;; extensions stepping on each others' toes.
(defclause ::default
  "If lhs is logically true, yield rhs, else proceed with clauses."
  [lhs rhs child]
  `(if ~lhs ~rhs ~child))

(defclause ::when
  "If rhs is logically true, proceed with clauses, else yield nil."
  [_ test child]
  `(when ~test ~child))

;; TODO better-cond's :when-let and similar support multiple forms and
;; :let itself
(defclause ::when-let
  "rhs is a binding vector per `clojure.core/when-let`. When its test
  is true, proceed with successive clauses with binding-form bound to
  the value of test, else yield nil."
  [_ binding child]
  `(when-let ~binding ~child))

(defclause ::when-some
  "rhs is a binding vector per `clojure.core/when-some`. When its test
   is not nil, proceed with successive clauses with binding-form bound
   to the value of test, else yield nil."
  [_ binding child]
  `(when-some ~binding ~child))

(defclause ::do
  "Evaluate rhs, presumably for side-effects, and proceed with
  successive clauses."
  [_ rhs child]
  `(do ~rhs ~child))

(defclause ::let
  "rhs is a bindings vector per `clojure.core/let`. Evaluates
  successive clauses in a lexical context in which the symbols in the
  binding-forms are bound to their respective init-exprs or parts
  therein."
  [_ bindings* child]
  `(let ~bindings* ~child))

