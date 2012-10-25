(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc n]
                 (cond 
                  (zero? n) acc
                  :else (recur (* acc base) (dec n))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [acc seq-]
                 (cond
                  (empty? seq-) acc
                  :else (recur (first seq-) (rest seq-))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (loop [seq-a seq1
         seq-b seq2]
    (cond 
     (and (empty? seq-a) (empty? seq-b)) true
    (= (first seq-a) (first seq-b))
   (seq= (rest seq-a) (rest seq-b))
     :else false)))

(defn find-first-index [pred a-seq]
  (loop [acc 0
         seq- a-seq]
    (cond
     (empty? seq-) nil
     (pred (first seq-)) acc
     :else (recur (inc acc) (rest seq-)))))

(defn avg [a-seq]
  (loop [acc 0
         ind 0
         seq- a-seq]
    (cond
     (empty? seq-) (/ acc ind)
     :else (recur (+ acc (first seq-)) (inc ind) (rest seq-)))))

(defn toggle [a-set elem]
  (cond
   (contains? a-set elem) (disj a-set elem)
   :else (conj a-set elem)))

(defn parity [a-seq]
  (loop [acc #{}
         seq- a-seq]
    (cond
     (empty? seq-) acc
     :else (recur (toggle acc (first seq-)) (rest seq-)))))

(defn fast-fibo [n]
  (loop [acc 2
         n-1 1
         n-2 0]
    (cond
     (== n 0) n
     (== n 1) n
     (== acc n) (+ n-1 n-2)
     :else (recur (inc acc) (+ n-1 n-2) n-1))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= elem (first a-seq)) true
   :else (sequence-contains? elem (rest a-seq))))

(defn vector-last [vect]
  (get vect (- (count vect) 1)))

(defn cut-at-repetition [a-seq]
  (loop [acc []
         seq- a-seq]
    (cond
     (empty? seq-) (reverse acc)
     (sequence-contains? (vector-last seq-) acc) acc
     :else (recur (conj acc (first seq-)) (rest seq-)))))