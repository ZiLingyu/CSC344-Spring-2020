;; check if a list is a sequence or just a symbol
(defn seq-or-sym? [x]
  (or (seq? x) (symbol? x)))

;; simplify and: 
 ; always return false is there is a false;
 ; return true if all are true
 ; if there is only one symbol, return the value of symbol (with infinite true)
 ; if there is multiple symbols, return the list of symbol (with infinite true)
(defn and-simplify [l]
  (if (= 'and (nth l 0)) 
  (cond
    (some false? (rest l)) false
    (every? true? (rest l)) true
    (= 1 (count (filter seq-or-sym? (distinct (rest l))))) (first (filter seq-or-sym? (rest l)))
    (some seq-or-sym? (rest l)) (filter seq-or-sym? (distinct l)))
l))

;; simplify or: 
 ; always return true is there is a ture;
 ; return false if all are false
 ; if there is only one symbol, return the value of symbol (with infinite false)
 ; if there is multiple symbols, return the list of symbol (with infinite false)
(defn or-simplify [l]
  (if (= 'or (nth l 0)) 
    (cond 
      (some true? (rest l)) true
      (every? false? (rest l)) false
      (= 1 (count (filter seq-or-sym? (distinct (rest l))))) (first (filter seq-or-sym? (rest l)))
      (some seq-or-sym? (rest l)) (filter seq-or-sym? (distinct l))
  )l))

;; not-simplify helper:
 ; applying the De Morgan's Law using not-simplify
(defn not-simplify-helper [l]
  (cond
    (= 'and (nth l 0)) (conj (map (fn [i] (simplify-n (list 'not i))) (rest l)) 'or )
    (= 'or (nth l 0)) (conj (map #(simplify-n (list 'not %)) (rest l)) 'and )
    (= 'not (nth l 0)) (second l)
))

;; simplify not: 
 ; if not true -> false
 ; if not false -> true
(defn not-simplify [l]
  (if (= 'not (nth l 0)) 
    (cond
      (every? false? (rest l)) true
      (every? true? (rest l)) false
      (seq? (second l)) (not-simplify-helper (second l)) 
      :else l  
)l))

;; simplify the expression not recursively
(defn simplify-n [l]
  (cond 
  		(= 'and (nth l 0)) (and-simplify l)
      (= 'or (nth l 0)) (or-simplify l)
      (= 'not (nth l 0)) (not-simplify l)
))

;; does the recursion with an anonymous function and simplify the expression from the inside out
(defn simplify [l]
  (if (not-any? seq? (rest l))
    (cond
      (= 'and (nth l 0)) (and-simplify l)
      (= 'or (nth l 0)) (or-simplify l)
      (= 'not (nth l 0)) (not-simplify l))
    (simplify-n
   		(map (fn [i]
    		(if (seq? i)
    			(simplify i)
      	  i
      ))l))
))

;; deep substitute 
(defn bind-values [m l]
  (map (fn [i]
    (if (seq? i)
      (bind-values m i)
      (m i i)
))l))

;; entire evaluation
(defn evalexp [exp bindings] (simplify (bind-values bindings exp)))

;; expression
(def p1 '(and x (or x (and y (not z)))))
(def p2 '(and (and z false)(or x true)))
(def p3 '(or true a))
(def p4 '(not (or (not x) x)))
