(ns extensible-cond.core
  (:refer-clojure :exclude [cond]))

(defmulti clause
  "Build a clause for `cond` for dispatch value `k`"
  (fn [k _ _] k)
  :default ::default
  ;; TODO consider alternate hierarchy
  )

;; TODO or should it be that non-keywords have this behavior? I think
;; no, because if people want arbitrary behavior based on the type or
;; value of lhs, then who am I to deny them? OTOH, pinning down the
;; non-keyword behavior would limit the possibility for different
;; extensions stepping on each others' toes.
(defmethod clause ::default [lhs rhs child]
  `(if ~lhs ~rhs ~child))
;; TODO (maybe) crazy idea: defclause, which would expand to
;; `(defmethod clause ...)` but would also alter `cond`'s docstring
;; with a provided docstring.

(defmethod clause ::when [_ test child]
  `(when ~test ~child))

;; TODO better-cond's :when-let and similar support multiple forms and
;; :let itself
(defmethod clause ::when-let [_ binding child]
  `(when-let ~binding ~child))

(defmethod clause ::when-some [_ binding child]
  `(when-some ~binding ~child))

(defmethod clause ::do [_ rhs child]
  `(do ~rhs ~child))

(defmethod clause ::let [_ bindings* child]
  `(let ~bindings* ~child))

(defmacro cond [& clauses]
  ;; TODO either handle implicit else or enforce even number of forms.
  (->> clauses
       (partition 2)
       reverse
       (reduce (fn [child [k rhs]] (clause k rhs child)) nil)))
