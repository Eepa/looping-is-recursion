(ns looping-is-recursion)

(defn power [a n]
  (let [helper (fn [a n result]
                 (if (zero? n)
                   result
                   (recur a (dec n) (* a result))))]
  (helper a n 1)))



(defn last-element [a-seq]
  (let [helper (fn [a-seq result]
                 (if (empty? a-seq)
                   result
                   (recur (rest a-seq) (first a-seq))))]
    (helper a-seq nil)))


(defn seq= [seq1 seq2]
  (let [helper (fn [seq1 seq2 result]
                 (if (empty? seq1)
                   (if (empty? seq2)
                     result
                     false)
                   (if (empty? seq2)
                     false
                     (recur (rest seq1) (rest seq2) (= (first seq1) (first seq2))))))]
    (helper seq1 seq2 true)))



(defn find-first-index [pred a-seq]
  (loop [index 0
         pred pred
         a-seq a-seq]
    (if (empty? a-seq)
      nil
      (if (pred (first a-seq))
        index
        (recur (+ index 1) pred (rest a-seq))))))


(defn avg [a-seq]
  (loop [items 0
         a-seq a-seq
         sum 0]
    (if (empty? a-seq)
      (/ sum items)
      (recur (+ items 1) (rest a-seq) (+ sum (first a-seq))))))


(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))


(defn parity [a-seq]
  (loop [new-set #{}
         a-seq a-seq]
    (if (empty? a-seq)
      new-set
      (recur (toggle new-set (first a-seq)) (rest a-seq)))))



(defn fast-fibo [n]
  (loop [Fn-1 1
         Fn 0
         n n]
    (if (zero? n)
      Fn
      (recur Fn (+ Fn-1 Fn) (dec n)))))


(defn cut-at-repetition [a-seq]
  (loop [encountered-things #{}
         a-seq a-seq
         result []]
    (if (empty? a-seq)
      result
      (if (contains? encountered-things (first a-seq))
        result
        (recur (conj encountered-things (first a-seq)) (rest a-seq) (conj result (first a-seq)))))))

