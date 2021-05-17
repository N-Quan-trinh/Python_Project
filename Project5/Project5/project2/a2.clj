(ns untitled2.core
  (:import (clojure.lang Symbol)))

;generates a list containing n number of lst items
(defn generate [n lst]
  (cond
    (= n 0) ()
    :else  (cons lst (generate (- n 1) lst))
    )
  )

;a general purpose method to construct an expression using a logical operator and other expressions
(defn createExpr [act lst]
  (cond
    (empty? lst) nil
    (empty? (rest lst)) (first lst)
    :else (list act (first lst) (createExpr act (rest lst)))
    )
  )

;create an expression using a logical and operator and other expressions
(defn andexp [& rest]
  (createExpr 'and rest)
  )

;create an expression using a logical or operator and other expressions
(defn orexp [& rest]
  (createExpr 'or rest)
  )

;create an expression using a logical not operator and an expression
(defn notexp [e1] (list 'not e1))



;get's the first list in the expression
(defn get-inner-list [expr]
  (cond
    (empty? expr) nil
    (and (empty? (rest expr)) (list? (first expr))) (first expr)
    :else (get-inner-list (rest expr))
    )
  )


(defn copy [expr]
  (if (empty? expr)
    nil
    (do
      (def default (into () (into () expr)))
      (cond
        (empty? default) nil
        (or (empty? (rest (rest default))) (list? (rest (rest default)))) default
        :else (do
                (if
                  (= (count default) 2)
                  (list (nth default 0) (copy (rest default)))
                  (list (nth default 0) (nth default 1) (copy (first (rest (rest default)))))
                  )
                )
        )
      )
    )
  )

;substitute all values to a key for a given expression
(defn deep-substitute [expr key-value-pair]
  (copy
    (map #(cond
            (seq? %) (deep-substitute % key-value-pair)
            :default (key-value-pair % %))
         expr
         )
    )
  )


;return the original expression with the values substituted in
(defn build-expression [expr keys-to-map hashmap]
  (cond
    (empty? keys-to-map) nil
    (empty? (rest keys-to-map)) (deep-substitute expr (hash-map (first keys-to-map) (get hashmap (first keys-to-map))))
    :else (build-expression (deep-substitute expr (hash-map (first keys-to-map) (get hashmap (first keys-to-map)))) (rest keys-to-map) hashmap)
    )
  )


(defn substitute [expr key-value-pair]
  (copy (map #(key-value-pair % %) expr))
  )

(defn replace-pred [expr pred repl]
  (cond
    (some? (some (fn [x] (= pred x)) (list expr))) (substitute (list expr) (hash-map pred repl))
    (some? (some (fn [x] (= pred x)) expr)) (substitute expr (hash-map pred repl))
    :else expr
    )
  )


(defn build-replace-expression [expression pred repl]
  (cond
    (not (list? expression)) expression
    (empty? (rest expression)) (first expression)
    (or (some? (some (fn [x] (= pred x)) (list expression))) (some? (some (fn [x] (= pred x)) expression))) (replace-pred expression pred repl)
    :else (cond
            (> (count expression) 2) (list (nth expression 0) (build-replace-expression (nth expression 1) pred repl) (build-replace-expression (nth expression 2) pred repl))
            :else (list (nth expression 0) (build-replace-expression (nth expression 1) pred repl))
            )
    )
  )


(defn pred-present [expr pred]
  (cond
    (or (some? (some (fn [x] (= pred x)) (list expr))) (some? (some (fn [x] (= pred x)) expr))) true
    :else false
    )
  )

(defn deep-search [expr pred]
  (cond
    (not (list? expr)) false
    (or (some? (some (fn [x] (= pred x)) (list expr))) (some? (some (fn [x] (= pred x)) expr))) (pred-present expr pred)
    :else (do
            (cond
              (> (count expr) 2) (or (deep-search (nth expr 1) pred) (deep-search (nth expr 2) pred))
              (> (count expr) 1) (deep-search (nth expr 1) pred)
              :else expr
              )
            )
    )
  )



;return the symbol in a list if one is there
(defn get-symbol-list [lst]
  (empty? (remove (fn[x] (= 'not x)) (remove (fn[x] (= 'or x)) (remove (fn[x] (= 'and x)) (remove false? (remove true? (flatten lst))))))) '()
  :else (copy (remove (fn[x] (= 'not x)) (remove (fn[x] (= 'or x)) (remove (fn[x] (= 'and x)) (remove false? (remove true? (flatten lst)))))))
  )


(defn get-neighbor [expr arg1 arg2]
  (cond
    (not (list? expr)) nil
    (and (some? (some (fn [x] (= arg1 x)) expr)) (some? (some (fn [x] (= arg2 x)) expr))) (first (remove (fn[x] (= arg1 x)) (remove (fn[x] (= arg2 x)) expr)))
    :else (cond
            (> (count expr) 2) (if (not (nil? (get-neighbor (nth expr 1) arg1 arg2)))
                                 (get-neighbor (nth expr 1) arg1 arg2)
                                 (get-neighbor (nth expr 2) arg1 arg2)
                                 )
            (> (count expr) 1)  (get-neighbor (nth expr 1) arg1 arg2)
            :else expr
            )
    )
  )


(defn find-neighbor [expr arg1 arg2]
  (cond
    (not (list? expr)) false
    (and (some? (some (fn [x] (= arg1 x)) expr)) (some? (some (fn [x] (= arg2 x)) expr))) true
    :else (cond
            (> (count expr) 2) (or (find-neighbor (nth expr 1) arg1 arg2) (find-neighbor (nth expr 2) arg1 arg2))
            (> (count expr) 1) (find-neighbor (nth expr 1) arg1 arg2)
            :else false
            )
    )
  )





;make a table with conditions to check for when simplifying the expressions
(defn property-simplification [expr]
  (def keys-to-map '())
  (if (not (nil? (get-symbol-list expr)))
    (def keys-to-map (distinct (get-symbol-list expr)))
    )

  (cond
    (= (count keys-to-map) 0) (cond
                                (or (= (count expr) 1) (not (list? expr))) expr

                                (and (and (= (count expr) 3) (find-neighbor expr 'or 'true)) (deep-search expr (list 'or 'true (get-neighbor expr 'or 'true)))) (property-simplification (build-replace-expression expr (list 'or 'true (get-neighbor expr 'or 'true)) 'true))
                                (and (and (= (count expr) 3) (find-neighbor expr 'true 'or)) (deep-search expr (list 'or (get-neighbor expr 'true 'or) 'true))) (property-simplification (build-replace-expression expr (list 'or (get-neighbor expr 'true 'or) 'true) 'true))

                                (and (and (= (count expr) 3) (find-neighbor expr 'or 'false)) (deep-search expr (list 'or 'false (get-neighbor expr 'or 'false)))) (property-simplification (build-replace-expression expr (list 'or 'false (get-neighbor expr 'or 'false)) (get-neighbor expr 'or 'false)))
                                (and (and (= (count expr) 3) (find-neighbor expr 'false 'or)) (deep-search expr (list 'or (get-neighbor expr 'false 'or) 'false))) (property-simplification (build-replace-expression expr (list 'or (get-neighbor expr 'false 'or) 'false) (get-neighbor expr 'false 'or)))

                                (and (and (= (count expr) 3) (find-neighbor expr 'and 'false)) (deep-search expr (list 'and (get-neighbor expr 'and 'false) 'false))) (property-simplification (build-replace-expression expr (list 'and (get-neighbor expr 'and 'false) 'false) 'false))
                                (and (and (= (count expr) 3) (find-neighbor expr 'false 'and)) (deep-search expr (list 'and 'false (get-neighbor expr 'false 'and)))) (property-simplification (build-replace-expression expr (list 'and 'false (get-neighbor expr 'false 'and)) 'false))

                                (and (and (= (count expr) 3) (find-neighbor expr 'and 'true)) (deep-search expr (list 'and (get-neighbor expr 'and 'true) 'true))) (property-simplification (build-replace-expression expr (list 'and (get-neighbor expr 'and 'true) 'true) (get-neighbor expr 'and 'true)))
                                (and (and (= (count expr) 3) (find-neighbor expr 'true 'and)) (deep-search expr (list 'and 'true (get-neighbor expr 'true 'and)))) (property-simplification (build-replace-expression expr (list 'and 'true (get-neighbor expr 'true 'and)) (get-neighbor expr 'true 'and)))

                                (deep-search expr '(or true)) (property-simplification (build-replace-expression expr '(or true) 'true))
                                (deep-search expr '(or false)) (property-simplification (build-replace-expression expr '(or false) 'false))
                                (deep-search expr '(and true)) (property-simplification (build-replace-expression expr '(and true) 'true))
                                (deep-search expr '(and false)) (property-simplification (build-replace-expression expr '(and false) 'false))

                                (deep-search expr '(or true true)) (property-simplification (build-replace-expression expr '(or true true) 'true))
                                (deep-search expr '(or false false)) (property-simplification (build-replace-expression expr '(or false false) 'false))
                                (deep-search expr '(or true false)) (property-simplification (build-replace-expression expr '(or true false) 'true))
                                (deep-search expr '(or false true)) (property-simplification (build-replace-expression expr '(or false true) 'true))

                                (deep-search expr '(and true true)) (property-simplification (build-replace-expression expr '(and true true) 'true))
                                (deep-search expr '(and false false)) (property-simplification (build-replace-expression expr '(and false false) 'false))
                                (deep-search expr '(and true false)) (property-simplification (build-replace-expression expr '(and true false) 'false))
                                (deep-search expr '(and false true)) (property-simplification (build-replace-expression expr '(and false true) 'false))

                                (deep-search expr '(not false)) (property-simplification (build-replace-expression expr '(not false) 'true))
                                (deep-search expr '(not true)) (property-simplification (build-replace-expression expr '(not true) 'false))
                                :else expr
                                )
    (= (count keys-to-map) 1) (cond
                                (or (= (count expr) 1) (not (list? expr))) expr

                                (and (and (= (count expr) 3) (find-neighbor expr 'or 'true)) (deep-search expr (list 'or 'true (get-neighbor expr 'or 'true)))) (property-simplification (build-replace-expression expr (list 'or 'true (get-neighbor expr 'or 'true)) 'true))
                                (and (and (= (count expr) 3) (find-neighbor expr 'true 'or)) (deep-search expr (list 'or (get-neighbor expr 'true 'or) 'true))) (property-simplification (build-replace-expression expr (list 'or (get-neighbor expr 'true 'or) 'true) 'true))

                                (and (and (= (count expr) 3) (find-neighbor expr 'or 'false)) (deep-search expr (list 'or 'false (get-neighbor expr 'or 'false)))) (property-simplification (build-replace-expression expr (list 'or 'false (get-neighbor expr 'or 'false)) (get-neighbor expr 'or 'false)))
                                (and (and (= (count expr) 3) (find-neighbor expr 'false 'or)) (deep-search expr (list 'or (get-neighbor expr 'false 'or) 'false))) (property-simplification (build-replace-expression expr (list 'or (get-neighbor expr 'false 'or) 'false) (get-neighbor expr 'false 'or)))

                                (and (and (= (count expr) 3) (find-neighbor expr 'and 'false)) (deep-search expr (list 'and (get-neighbor expr 'and 'false) 'false))) (property-simplification (build-replace-expression expr (list 'and (get-neighbor expr 'and 'false) 'false) 'false))
                                (and (and (= (count expr) 3) (find-neighbor expr 'false 'and)) (deep-search expr (list 'and 'false (get-neighbor expr 'false 'and)))) (property-simplification (build-replace-expression expr (list 'and 'false (get-neighbor expr 'false 'and)) 'false))

                                (and (and (= (count expr) 3) (find-neighbor expr 'and 'true)) (deep-search expr (list 'and (get-neighbor expr 'and 'true) 'true))) (property-simplification (build-replace-expression expr (list 'and (get-neighbor expr 'and 'true) 'true) (get-neighbor expr 'and 'true)))
                                (and (and (= (count expr) 3) (find-neighbor expr 'true 'and)) (deep-search expr (list 'and 'true (get-neighbor expr 'true 'and)))) (property-simplification (build-replace-expression expr (list 'and 'true (get-neighbor expr 'true 'and)) (get-neighbor expr 'true 'and)))

                                (deep-search expr '(or true)) (property-simplification (build-replace-expression expr '(or true) 'true))
                                (deep-search expr '(or false)) (property-simplification (build-replace-expression expr '(or false) 'false))
                                (deep-search expr '(and true)) (property-simplification (build-replace-expression expr '(and true) 'true))
                                (deep-search expr '(and false)) (property-simplification (build-replace-expression expr '(and false) 'false))

                                (deep-search expr (list 'or (nth keys-to-map 0) (nth keys-to-map 0))) (property-simplification (build-replace-expression expr (list 'or (nth keys-to-map 0) (nth keys-to-map 0)) (nth keys-to-map 0)))

                                (deep-search expr '(or true true)) (property-simplification (build-replace-expression expr '(or true true) 'true))
                                (deep-search expr '(or false false)) (property-simplification (build-replace-expression expr '(or false false) 'false))
                                (deep-search expr '(or true false)) (property-simplification (build-replace-expression expr '(or true false) 'true))
                                (deep-search expr '(or false true)) (property-simplification (build-replace-expression expr '(or false true) 'true))

                                (deep-search expr '(and true true)) (property-simplification (build-replace-expression expr '(and true true) 'true))
                                (deep-search expr '(and false false)) (property-simplification (build-replace-expression expr '(and false false) 'false))
                                (deep-search expr '(and true false)) (property-simplification (build-replace-expression expr '(and true false) 'false))
                                (deep-search expr '(and false true)) (property-simplification (build-replace-expression expr '(and false true) 'false))

                                (deep-search expr '(not false)) (property-simplification (build-replace-expression expr '(not false) 'true))
                                (deep-search expr '(not true)) (property-simplification (build-replace-expression expr '(not true) 'false))

                                (deep-search expr (list 'or (nth keys-to-map 0) 'false)) (property-simplification (build-replace-expression expr (list 'or (nth keys-to-map 0) 'false) (nth keys-to-map 0)))
                                (deep-search expr (list 'or 'false (nth keys-to-map 0))) (property-simplification (build-replace-expression expr (list 'or 'false (nth keys-to-map 0)) (nth keys-to-map 0)))
                                (deep-search expr (list 'or 'true (nth keys-to-map 0))) (property-simplification (build-replace-expression expr (list 'or 'true (nth keys-to-map 0)) 'true))
                                (deep-search expr (list 'or (nth keys-to-map 0) 'true)) (property-simplification (build-replace-expression expr (list 'or (nth keys-to-map 0) 'true) 'true))

                                (deep-search expr (list 'and (nth keys-to-map 0) 'false)) (property-simplification (build-replace-expression expr (list 'and (nth keys-to-map 0) 'false) 'false))
                                (deep-search expr (list 'and 'false (nth keys-to-map 0))) (property-simplification (build-replace-expression expr (list 'and 'false (nth keys-to-map 0)) 'false))
                                (deep-search expr (list 'and (nth keys-to-map 0) 'true)) (property-simplification (build-replace-expression expr (list 'and (nth keys-to-map 0) 'true) (nth keys-to-map 0)))
                                (deep-search expr (list 'and 'true (nth keys-to-map 0))) (property-simplification (build-replace-expression expr (list 'and 'true (nth keys-to-map 0)) (nth keys-to-map 0)))
                                :else expr
                                )
    (= (count keys-to-map) 2) (cond
                                (or (= (count expr) 1) (not (list? expr))) expr

                                (and (and (= (count expr) 3) (find-neighbor expr 'or 'true)) (deep-search expr (list 'or 'true (get-neighbor expr 'or 'true)))) (property-simplification (build-replace-expression expr (list 'or 'true (get-neighbor expr 'or 'true)) 'true))
                                (and (and (= (count expr) 3) (find-neighbor expr 'true 'or)) (deep-search expr (list 'or (get-neighbor expr 'true 'or) 'true))) (property-simplification (build-replace-expression expr (list 'or (get-neighbor expr 'true 'or) 'true) 'true))

                                (and (and (= (count expr) 3) (find-neighbor expr 'or 'false)) (deep-search expr (list 'or 'false (get-neighbor expr 'or 'false)))) (property-simplification (build-replace-expression expr (list 'or 'false (get-neighbor expr 'or 'false)) (get-neighbor expr 'or 'false)))
                                (and (and (= (count expr) 3) (find-neighbor expr 'false 'or)) (deep-search expr (list 'or (get-neighbor expr 'false 'or) 'false))) (property-simplification (build-replace-expression expr (list 'or (get-neighbor expr 'false 'or) 'false) (get-neighbor expr 'false 'or)))

                                (and (and (= (count expr) 3) (find-neighbor expr 'and 'false)) (deep-search expr (list 'and (get-neighbor expr 'and 'false) 'false))) (property-simplification (build-replace-expression expr (list 'and (get-neighbor expr 'and 'false) 'false) 'false))
                                (and (and (= (count expr) 3) (find-neighbor expr 'false 'and)) (deep-search expr (list 'and 'false (get-neighbor expr 'false 'and)))) (property-simplification (build-replace-expression expr (list 'and 'false (get-neighbor expr 'false 'and)) 'false))

                                (and (and (= (count expr) 3) (find-neighbor expr 'and 'true)) (deep-search expr (list 'and (get-neighbor expr 'and 'true) 'true))) (property-simplification (build-replace-expression expr (list 'and (get-neighbor expr 'and 'true) 'true) (get-neighbor expr 'and 'true)))
                                (and (and (= (count expr) 3) (find-neighbor expr 'true 'and)) (deep-search expr (list 'and 'true (get-neighbor expr 'true 'and)))) (property-simplification (build-replace-expression expr (list 'and 'true (get-neighbor expr 'true 'and)) (get-neighbor expr 'true 'and)))

                                (deep-search expr '(or true)) (property-simplification (build-replace-expression expr '(or true) 'true))
                                (deep-search expr '(or false)) (property-simplification (build-replace-expression expr '(or false) 'false))
                                (deep-search expr '(and true)) (property-simplification (build-replace-expression expr '(and true) 'true))
                                (deep-search expr '(and false)) (property-simplification (build-replace-expression expr '(and false) 'false))

                                (deep-search expr (list 'or (nth keys-to-map 0) (nth keys-to-map 0))) (property-simplification (build-replace-expression expr (list 'or (nth keys-to-map 0) (nth keys-to-map 0)) (nth keys-to-map 0)))
                                (deep-search expr (list 'or (nth keys-to-map 1) (nth keys-to-map 1))) (property-simplification (build-replace-expression expr (list 'or (nth keys-to-map 1) (nth keys-to-map 1)) (nth keys-to-map 1)))

                                (deep-search expr '(or true true)) (property-simplification (build-replace-expression expr '(or true true) 'true))
                                (deep-search expr '(or false false)) (property-simplification (build-replace-expression expr '(or false false) 'false))
                                (deep-search expr '(or true false)) (property-simplification (build-replace-expression expr '(or true false) 'true))
                                (deep-search expr '(or false true)) (property-simplification (build-replace-expression expr '(or false true) 'true))

                                (deep-search expr '(and true true)) (property-simplification (build-replace-expression expr '(and true true) 'true))
                                (deep-search expr '(and false false)) (property-simplification (build-replace-expression expr '(and false false) 'false))
                                (deep-search expr '(and true false)) (property-simplification (build-replace-expression expr '(and true false) 'false))
                                (deep-search expr '(and false true)) (property-simplification (build-replace-expression expr '(and false true) 'false))

                                (deep-search expr '(not false)) (property-simplification (build-replace-expression expr '(not false) 'true))
                                (deep-search expr '(not true)) (property-simplification (build-replace-expression expr '(not true) 'false))

                                (deep-search expr (list 'or (nth keys-to-map 0) 'false)) (property-simplification (build-replace-expression expr (list 'or (nth keys-to-map 0) 'false) (nth keys-to-map 0)))
                                (deep-search expr (list 'or 'false (nth keys-to-map 0))) (property-simplification (build-replace-expression expr (list 'or 'false (nth keys-to-map 0)) (nth keys-to-map 0)))
                                (deep-search expr (list 'or 'true (nth keys-to-map 0))) (property-simplification (build-replace-expression expr (list 'or 'true (nth keys-to-map 0)) 'true))
                                (deep-search expr (list 'or (nth keys-to-map 0) 'true)) (property-simplification (build-replace-expression expr (list 'or (nth keys-to-map 0) 'true) 'true))

                                (deep-search expr (list 'and (nth keys-to-map 0) 'false)) (property-simplification (build-replace-expression expr (list 'and (nth keys-to-map 0) 'false) 'false))
                                (deep-search expr (list 'and 'false (nth keys-to-map 0))) (property-simplification (build-replace-expression expr (list 'and 'false (nth keys-to-map 0)) 'false))
                                (deep-search expr (list 'and (nth keys-to-map 0) 'true)) (property-simplification (build-replace-expression expr (list 'and (nth keys-to-map 0) 'true) (nth keys-to-map 0)))
                                (deep-search expr (list 'and 'true (nth keys-to-map 0))) (property-simplification (build-replace-expression expr (list 'and 'true (nth keys-to-map 0)) (nth keys-to-map 0)))

                                (deep-search expr (list 'or (nth keys-to-map 1) 'false)) (property-simplification (build-replace-expression expr (list 'or (nth keys-to-map 1) 'false) (nth keys-to-map 1)))
                                (deep-search expr (list 'or 'false (nth keys-to-map 1))) (property-simplification (build-replace-expression expr (list 'or 'false (nth keys-to-map 1)) (nth keys-to-map 1)))
                                (deep-search expr (list 'or 'true (nth keys-to-map 1))) (property-simplification (build-replace-expression expr (list 'or 'true (nth keys-to-map 1)) 'true))
                                (deep-search expr (list 'or (nth keys-to-map 1) 'true)) (property-simplification (build-replace-expression expr (list 'or (nth keys-to-map 1) 'true) 'true))

                                (deep-search expr (list 'and (nth keys-to-map 1) 'false)) (property-simplification (build-replace-expression expr (list 'and (nth keys-to-map 1) 'false) 'false))
                                (deep-search expr (list 'and 'false (nth keys-to-map 1))) (property-simplification (build-replace-expression expr (list 'and 'false (nth keys-to-map 1)) 'false))
                                (deep-search expr (list 'and (nth keys-to-map 1) 'true)) (property-simplification (build-replace-expression expr (list 'and (nth keys-to-map 1) 'true) (nth keys-to-map 1)))
                                (deep-search expr (list 'and 'true (nth keys-to-map 1))) (property-simplification (build-replace-expression expr (list 'and 'true (nth keys-to-map 1)) (nth keys-to-map 1)))



                                (deep-search expr (list 'not (list 'and (nth keys-to-map 0) (nth keys-to-map 1)))) (property-simplification (build-replace-expression expr (list 'not (list 'and (nth keys-to-map 0) (nth keys-to-map 1))) (list 'or (list 'not (nth keys-to-map 0)) (list 'not (nth keys-to-map 1)))))
                                (deep-search expr (list 'not (list 'and (nth keys-to-map 1) (nth keys-to-map 0)))) (property-simplification (build-replace-expression expr (list 'not (list 'and (nth keys-to-map 1) (nth keys-to-map 0))) (list 'or (list 'not (nth keys-to-map 1)) (list 'not (nth keys-to-map 0)))))

                                (deep-search expr (list 'not (list 'or (nth keys-to-map 0) (nth keys-to-map 1)))) (property-simplification (build-replace-expression expr (list 'not (list 'or (nth keys-to-map 0) (nth keys-to-map 1))) (list 'and (list 'not (nth keys-to-map 0)) (list 'not (nth keys-to-map 1)))))
                                (deep-search expr (list 'not (list 'or (nth keys-to-map 1) (nth keys-to-map 0)))) (property-simplification (build-replace-expression expr (list 'not (list 'or (nth keys-to-map 1) (nth keys-to-map 0))) (list 'and (list 'not (nth keys-to-map 1)) (list 'not (nth keys-to-map 0)))))
                                :else expr
                                )
    (= (count keys-to-map) 3) (cond
                                (or (= (count expr) 1) (not (list? expr))) expr

                                (and (and (= (count expr) 3) (find-neighbor expr 'or 'true)) (deep-search expr (list 'or 'true (get-neighbor expr 'or 'true)))) (property-simplification (build-replace-expression expr (list 'or 'true (get-neighbor expr 'or 'true)) 'true))
                                (and (and (= (count expr) 3) (find-neighbor expr 'true 'or)) (deep-search expr (list 'or (get-neighbor expr 'true 'or) 'true))) (property-simplification (build-replace-expression expr (list 'or (get-neighbor expr 'true 'or) 'true) 'true))

                                (and (and (= (count expr) 3) (find-neighbor expr 'or 'false)) (deep-search expr (list 'or 'false (get-neighbor expr 'or 'false)))) (property-simplification (build-replace-expression expr (list 'or 'false (get-neighbor expr 'or 'false)) (get-neighbor expr 'or 'false)))
                                (and (and (= (count expr) 3) (find-neighbor expr 'false 'or)) (deep-search expr (list 'or (get-neighbor expr 'false 'or) 'false))) (property-simplification (build-replace-expression expr (list 'or (get-neighbor expr 'false 'or) 'false) (get-neighbor expr 'false 'or)))

                                (and (and (= (count expr) 3) (find-neighbor expr 'and 'false)) (deep-search expr (list 'and (get-neighbor expr 'and 'false) 'false))) (property-simplification (build-replace-expression expr (list 'and (get-neighbor expr 'and 'false) 'false) 'false))
                                (and (and (= (count expr) 3) (find-neighbor expr 'false 'and)) (deep-search expr (list 'and 'false (get-neighbor expr 'false 'and)))) (property-simplification (build-replace-expression expr (list 'and 'false (get-neighbor expr 'false 'and)) 'false))

                                (and (and (= (count expr) 3) (find-neighbor expr 'and 'true)) (deep-search expr (list 'and (get-neighbor expr 'and 'true) 'true))) (property-simplification (build-replace-expression expr (list 'and (get-neighbor expr 'and 'true) 'true) (get-neighbor expr 'and 'true)))
                                (and (and (= (count expr) 3) (find-neighbor expr 'true 'and)) (deep-search expr (list 'and 'true (get-neighbor expr 'true 'and)))) (property-simplification (build-replace-expression expr (list 'and 'true (get-neighbor expr 'true 'and)) (get-neighbor expr 'true 'and)))

                                (deep-search expr '(or true)) (property-simplification (build-replace-expression expr '(or true) 'true))
                                (deep-search expr '(or false)) (property-simplification (build-replace-expression expr '(or false) 'false))
                                (deep-search expr '(and true)) (property-simplification (build-replace-expression expr '(and true) 'true))
                                (deep-search expr '(and false)) (property-simplification (build-replace-expression expr '(and false) 'false))

                                (deep-search expr (list 'or (nth keys-to-map 0) (nth keys-to-map 0))) (property-simplification (build-replace-expression expr (list 'or (nth keys-to-map 0) (nth keys-to-map 0)) (nth keys-to-map 0)))
                                (deep-search expr (list 'or (nth keys-to-map 1) (nth keys-to-map 1))) (property-simplification (build-replace-expression expr (list 'or (nth keys-to-map 1) (nth keys-to-map 1)) (nth keys-to-map 1)))
                                (deep-search expr (list 'or (nth keys-to-map 2) (nth keys-to-map 2))) (property-simplification (build-replace-expression expr (list 'or (nth keys-to-map 2) (nth keys-to-map 2)) (nth keys-to-map 2)))

                                (deep-search expr '(or true true)) (property-simplification (build-replace-expression expr '(or true true) 'true))
                                (deep-search expr '(or false false)) (property-simplification (build-replace-expression expr '(or false false) 'false))
                                (deep-search expr '(or true false)) (property-simplification (build-replace-expression expr '(or true false) 'true))
                                (deep-search expr '(or false true)) (property-simplification (build-replace-expression expr '(or false true) 'true))

                                (deep-search expr '(and true true)) (property-simplification (build-replace-expression expr '(and true true) 'true))
                                (deep-search expr '(and false false)) (property-simplification (build-replace-expression expr '(and false false) 'false))
                                (deep-search expr '(and true false)) (property-simplification (build-replace-expression expr '(and true false) 'false))
                                (deep-search expr '(and false true)) (property-simplification (build-replace-expression expr '(and false true) 'false))

                                (deep-search expr '(not false)) (property-simplification (build-replace-expression expr '(not false) 'true))
                                (deep-search expr '(not true)) (property-simplification (build-replace-expression expr '(not true) 'false))

                                (deep-search expr (list 'or (nth keys-to-map 0) 'false)) (property-simplification (build-replace-expression expr (list 'or (nth keys-to-map 0) 'false) (nth keys-to-map 0)))
                                (deep-search expr (list 'or 'false (nth keys-to-map 0))) (property-simplification (build-replace-expression expr (list 'or 'false (nth keys-to-map 0)) (nth keys-to-map 0)))
                                (deep-search expr (list 'or 'true (nth keys-to-map 0))) (property-simplification (build-replace-expression expr (list 'or 'true (nth keys-to-map 0)) 'true))
                                (deep-search expr (list 'or (nth keys-to-map 0) 'true)) (property-simplification (build-replace-expression expr (list 'or (nth keys-to-map 0) 'true) 'true))

                                (deep-search expr (list 'and (nth keys-to-map 0) 'false)) (property-simplification (build-replace-expression expr (list 'and (nth keys-to-map 0) 'false) 'false))
                                (deep-search expr (list 'and 'false (nth keys-to-map 0))) (property-simplification (build-replace-expression expr (list 'and 'false (nth keys-to-map 0)) 'false))
                                (deep-search expr (list 'and (nth keys-to-map 0) 'true)) (property-simplification (build-replace-expression expr (list 'and (nth keys-to-map 0) 'true) (nth keys-to-map 0)))
                                (deep-search expr (list 'and 'true (nth keys-to-map 0))) (property-simplification (build-replace-expression expr (list 'and 'true (nth keys-to-map 0)) (nth keys-to-map 0)))

                                (deep-search expr (list 'or (nth keys-to-map 1) 'false)) (property-simplification (build-replace-expression expr (list 'or (nth keys-to-map 1) 'false) (nth keys-to-map 1)))
                                (deep-search expr (list 'or 'false (nth keys-to-map 1))) (property-simplification (build-replace-expression expr (list 'or 'false (nth keys-to-map 1)) (nth keys-to-map 1)))
                                (deep-search expr (list 'or 'true (nth keys-to-map 1))) (property-simplification (build-replace-expression expr (list 'or 'true (nth keys-to-map 1)) 'true))
                                (deep-search expr (list 'or (nth keys-to-map 1) 'true)) (property-simplification (build-replace-expression expr (list 'or (nth keys-to-map 1) 'true) 'true))

                                (deep-search expr (list 'and (nth keys-to-map 1) 'false)) (property-simplification (build-replace-expression expr (list 'and (nth keys-to-map 1) 'false) 'false))
                                (deep-search expr (list 'and 'false (nth keys-to-map 1))) (property-simplification (build-replace-expression expr (list 'and 'false (nth keys-to-map 1)) 'false))
                                (deep-search expr (list 'and (nth keys-to-map 1) 'true)) (property-simplification (build-replace-expression expr (list 'and (nth keys-to-map 1) 'true) (nth keys-to-map 1)))
                                (deep-search expr (list 'and 'true (nth keys-to-map 1))) (property-simplification (build-replace-expression expr (list 'and 'true (nth keys-to-map 1)) (nth keys-to-map 1)))

                                (deep-search expr (list 'or (nth keys-to-map 2) 'false)) (property-simplification (build-replace-expression expr (list 'or (nth keys-to-map 2) 'false) (nth keys-to-map 2)))
                                (deep-search expr (list 'or 'false (nth keys-to-map 2))) (property-simplification (build-replace-expression expr (list 'or 'false (nth keys-to-map 2)) (nth keys-to-map 2)))
                                (deep-search expr (list 'or 'true (nth keys-to-map 2))) (property-simplification (build-replace-expression expr (list 'or 'true (nth keys-to-map 2)) 'true))
                                (deep-search expr (list 'or (nth keys-to-map 2) 'true)) (property-simplification (build-replace-expression expr (list 'or (nth keys-to-map 2) 'true) 'true))

                                (deep-search expr (list 'and (nth keys-to-map 2) 'false)) (property-simplification (build-replace-expression expr (list 'and (nth keys-to-map 2) 'false) 'false))
                                (deep-search expr (list 'and 'false (nth keys-to-map 2))) (property-simplification (build-replace-expression expr (list 'and 'false (nth keys-to-map 2)) 'false))
                                (deep-search expr (list 'and (nth keys-to-map 2) 'true)) (property-simplification (build-replace-expression expr (list 'and (nth keys-to-map 2) 'true) (nth keys-to-map 2)))
                                (deep-search expr (list 'and 'true (nth keys-to-map 2))) (property-simplification (build-replace-expression expr (list 'and 'true (nth keys-to-map 2)) (nth keys-to-map 2)))



                                (deep-search expr (list 'not (list 'and (nth keys-to-map 0) (nth keys-to-map 1)))) (property-simplification (build-replace-expression expr (list 'not (list 'and (nth keys-to-map 0) (nth keys-to-map 1))) (list 'or (list 'not (nth keys-to-map 0)) (list 'not (nth keys-to-map 1)))))
                                (deep-search expr (list 'not (list 'and (nth keys-to-map 1) (nth keys-to-map 0)))) (property-simplification (build-replace-expression expr (list 'not (list 'and (nth keys-to-map 1) (nth keys-to-map 0))) (list 'or (list 'not (nth keys-to-map 1)) (list 'not (nth keys-to-map 0)))))

                                (deep-search expr (list 'not (list 'or (nth keys-to-map 0) (nth keys-to-map 1)))) (property-simplification (build-replace-expression expr (list 'not (list 'or (nth keys-to-map 0) (nth keys-to-map 1))) (list 'and (list 'not (nth keys-to-map 0)) (list 'not (nth keys-to-map 1)))))
                                (deep-search expr (list 'not (list 'or (nth keys-to-map 1) (nth keys-to-map 0)))) (property-simplification (build-replace-expression expr (list 'not (list 'or (nth keys-to-map 1) (nth keys-to-map 0))) (list 'and (list 'not (nth keys-to-map 1)) (list 'not (nth keys-to-map 0)))))

                                (deep-search expr (list 'not (list 'and (nth keys-to-map 1) (nth keys-to-map 2)))) (property-simplification (build-replace-expression expr (list 'not (list 'and (nth keys-to-map 1) (nth keys-to-map 2))) (list 'or (list 'not (nth keys-to-map 1)) (list 'not (nth keys-to-map 2)))))
                                (deep-search expr (list 'not (list 'and (nth keys-to-map 2) (nth keys-to-map 1)))) (property-simplification (build-replace-expression expr (list 'not (list 'and (nth keys-to-map 2) (nth keys-to-map 1))) (list 'or (list 'not (nth keys-to-map 2)) (list 'not (nth keys-to-map 1)))))

                                (deep-search expr (list 'not (list 'or (nth keys-to-map 1) (nth keys-to-map 2)))) (property-simplification (build-replace-expression expr (list 'not (list 'or (nth keys-to-map 1) (nth keys-to-map 2))) (list 'and (list 'not (nth keys-to-map 1)) (list 'not (nth keys-to-map 2)))))
                                (deep-search expr (list 'not (list 'or (nth keys-to-map 2) (nth keys-to-map 1)))) (property-simplification (build-replace-expression expr (list 'not (list 'or (nth keys-to-map 2) (nth keys-to-map 1))) (list 'and (list 'not (nth keys-to-map 2)) (list 'not (nth keys-to-map 1)))))

                                (deep-search expr (list 'not (list 'and (nth keys-to-map 0) (nth keys-to-map 2)))) (property-simplification (build-replace-expression expr (list 'not (list 'and (nth keys-to-map 0) (nth keys-to-map 2))) (list 'or (list 'not (nth keys-to-map 0)) (list 'not (nth keys-to-map 2)))))
                                (deep-search expr (list 'not (list 'and (nth keys-to-map 2) (nth keys-to-map 0)))) (property-simplification (build-replace-expression expr (list 'not (list 'and (nth keys-to-map 2) (nth keys-to-map 0))) (list 'or (list 'not (nth keys-to-map 2)) (list 'not (nth keys-to-map 0)))))

                                (deep-search expr (list 'not (list 'or (nth keys-to-map 0) (nth keys-to-map 2)))) (property-simplification (build-replace-expression expr (list 'not (list 'or (nth keys-to-map 0) (nth keys-to-map 2))) (list 'and (list 'not (nth keys-to-map 0)) (list 'not (nth keys-to-map 2)))))
                                (deep-search expr (list 'not (list 'or (nth keys-to-map 2) (nth keys-to-map 0)))) (property-simplification (build-replace-expression expr (list 'not (list 'or (nth keys-to-map 2) (nth keys-to-map 0))) (list 'and (list 'not (nth keys-to-map 2)) (list 'not (nth keys-to-map 0)))))



                                (deep-search expr (list 'or (nth keys-to-map 0) (nth keys-to-map 1) (nth keys-to-map 2))) (property-simplification (build-replace-expression expr (list 'or (nth keys-to-map 0) (nth keys-to-map 1) (nth keys-to-map 2)) (list 'or (nth keys-to-map 0) (list 'or (nth keys-to-map 1) (nth keys-to-map 2)))))
                                (deep-search expr (list 'or (nth keys-to-map 0) (nth keys-to-map 2) (nth keys-to-map 1))) (property-simplification (build-replace-expression expr (list 'or (nth keys-to-map 0) (nth keys-to-map 2) (nth keys-to-map 1)) (list 'or (nth keys-to-map 0) (list 'or (nth keys-to-map 2) (nth keys-to-map 1)))))
                                (deep-search expr (list 'or (nth keys-to-map 2) (nth keys-to-map 1) (nth keys-to-map 0))) (property-simplification (build-replace-expression expr (list 'or (nth keys-to-map 2) (nth keys-to-map 1) (nth keys-to-map 0)) (list 'or (nth keys-to-map 2) (list 'or (nth keys-to-map 1) (nth keys-to-map 0)))))
                                (deep-search expr (list 'or (nth keys-to-map 2) (nth keys-to-map 0) (nth keys-to-map 1))) (property-simplification (build-replace-expression expr (list 'or (nth keys-to-map 2) (nth keys-to-map 0) (nth keys-to-map 1)) (list 'or (nth keys-to-map 2) (list 'or (nth keys-to-map 0) (nth keys-to-map 1)))))
                                (deep-search expr (list 'or (nth keys-to-map 1) (nth keys-to-map 0) (nth keys-to-map 2))) (property-simplification (build-replace-expression expr (list 'or (nth keys-to-map 1) (nth keys-to-map 0) (nth keys-to-map 2)) (list 'or (nth keys-to-map 1) (list 'or (nth keys-to-map 0) (nth keys-to-map 2)))))
                                (deep-search expr (list 'or (nth keys-to-map 1) (nth keys-to-map 2) (nth keys-to-map 0))) (property-simplification (build-replace-expression expr (list 'or (nth keys-to-map 1) (nth keys-to-map 2) (nth keys-to-map 0)) (list 'or (nth keys-to-map 1) (list 'or (nth keys-to-map 2) (nth keys-to-map 0)))))

                                (deep-search expr (list 'and (nth keys-to-map 0) (nth keys-to-map 1) (nth keys-to-map 2))) (property-simplification (build-replace-expression expr (list 'and (nth keys-to-map 0) (nth keys-to-map 1) (nth keys-to-map 2)) (list 'and (nth keys-to-map 0) (list 'and (nth keys-to-map 1) (nth keys-to-map 2)))))
                                (deep-search expr (list 'and (nth keys-to-map 0) (nth keys-to-map 2) (nth keys-to-map 1))) (property-simplification (build-replace-expression expr (list 'and (nth keys-to-map 0) (nth keys-to-map 2) (nth keys-to-map 1)) (list 'and (nth keys-to-map 0) (list 'and (nth keys-to-map 2) (nth keys-to-map 1)))))
                                (deep-search expr (list 'and (nth keys-to-map 2) (nth keys-to-map 1) (nth keys-to-map 0))) (property-simplification (build-replace-expression expr (list 'and (nth keys-to-map 2) (nth keys-to-map 1) (nth keys-to-map 0)) (list 'and (nth keys-to-map 2) (list 'and (nth keys-to-map 1) (nth keys-to-map 0)))))
                                (deep-search expr (list 'and (nth keys-to-map 2) (nth keys-to-map 0) (nth keys-to-map 1))) (property-simplification (build-replace-expression expr (list 'and (nth keys-to-map 2) (nth keys-to-map 0) (nth keys-to-map 1)) (list 'and (nth keys-to-map 2) (list 'and (nth keys-to-map 0) (nth keys-to-map 1)))))
                                (deep-search expr (list 'and (nth keys-to-map 1) (nth keys-to-map 0) (nth keys-to-map 2))) (property-simplification (build-replace-expression expr (list 'and (nth keys-to-map 1) (nth keys-to-map 0) (nth keys-to-map 2)) (list 'and (nth keys-to-map 1) (list 'and (nth keys-to-map 0) (nth keys-to-map 2)))))
                                (deep-search expr (list 'and (nth keys-to-map 1) (nth keys-to-map 2) (nth keys-to-map 0))) (property-simplification (build-replace-expression expr (list 'and (nth keys-to-map 1) (nth keys-to-map 2) (nth keys-to-map 0)) (list 'and (nth keys-to-map 1) (list 'and (nth keys-to-map 2) (nth keys-to-map 0)))))
                                :else expr
                                )
    :else expr
    )
  )




(defn simplify [expr bindings]
  (def keys-to-map (keys bindings))
  (def new-expr (build-expression expr keys-to-map bindings))

  (def keys-to-map (get-symbol-list new-expr))

  (if (empty? keys-to-map)
    (eval new-expr)
    )

  (property-simplification new-expr)
  )


;evaluate any expression with its repective bindings
(defn evalexp [expr bindings]
  (simplify expr bindings)
  )
(def p1 '(and x (or x (and y (not z)))))
(println (evalexp p1 '{z false}))
(println (evalexp '(and true (or (or false x) (and y (or z z)))) '{x true, y false}))
(println (evalexp '(and true (or (or false z) (and y (or z z)))) '{x true, y false}))
(println (evalexp '(not (and true (or (or false z) (and y (or z z))))) '{x true, y false}))
