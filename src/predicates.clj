(ns predicates)

(defn sum-f [f g x]
  (+ (f x) (g x)))

(defn greater-than [n]
  (fn [k] (> k n)))

(defn less-than [n]
  (fn [k] (< k n)))

(defn equal-to [n]
  (fn [k]
    (if (number? n)
      (== k n)
      (= k n))))

(defn key->predicate [a-key]
  (fn [a-map] (contains? a-map a-key)))

(defn set->predicate [a-set]
  (fn [x] (contains? a-set x)))

(defn non-negatives [a-seq]
  (filter (complement neg?) a-seq))

(defn pred-and [pred1 pred2]
  (fn [x]
    (and (pred1 x)
         (pred2 x))))

(defn pred-or [pred1 pred2]
  (fn [x]
    (or (pred1 x)
        (pred2 x))))

(defn whitespace? [character]
  (Character/isWhitespace character))

(defn blank? [string]
  (every? whitespace? string))

(defn has-award? [book award]
  (contains? (book :awards) award))

(defn HAS-ALL-THE-AWARDS? [book awards]
  (every? (fn [x] (has-award? book x)) awards))

(defn my-some [pred a-seq]
  (let [results (filter pred a-seq)]
    (if (empty? results)
      nil
      (pred (first results)))))

(defn my-every? [pred a-seq]
  (if (boolean (my-some
               (complement pred)
               a-seq))
    false
    true))

(defn prime? [n]
  (let [divides? (fn [dividend]
                   (fn [divisor]
                     (= 0
                        (mod dividend divisor))))
        divisors (range 2 (inc (Math/sqrt n)))
        divides-n? (divides? n)]
    (cond (< n 2) false
          (= n 2) true
          :else (every? (complement divides-n?)
                       divisors))))
;^^
