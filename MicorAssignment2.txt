(defn and-simplify [l]
  (if (= 'and (nth l 0)) 
  (cond
    (some false? (rest l)) false
    (every? true? (rest l)) true
    (= 1 (count (filter symbol? (rest l)))) (first (filter symbol? (rest l)))
    (some symbol? (rest l)) (filter symbol? l)
    )l))



