(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst] card 
        rank1 {\T 10 \J 11 \Q 12 \K 13 \A 14 }]
    (if (Character/isDigit fst) (Integer/valueOf (str fst)) (rank1 fst))))

(defn suit [card]
  (let [[_ snd] card] (str snd)))

(defn pair? [hand]
(not (empty? (filter (fn [x] (== x 2))
(vals (frequencies (map rank hand)))))))

(defn three-of-a-kind? [hand]
  (not (empty? (filter (fn [x] (== x 3))
                       (vals (frequencies (map rank hand)))))))

(defn four-of-a-kind? [hand]

  (not (empty? (filter (fn [x] (== x 4))
                       (vals (frequencies (map rank hand)))))))

(defn flush? [hand]
                                        ; this solution is so freakin dumb 
  (let [number (str (suit (apply concat (frequencies (map suit hand)))))] (= (Integer/valueOf number) 5)))

(defn flush2? [hand]
  (let [suit1 (map suit flush-hand)] (apply = suit1)))

(defn full-house? [hand]
                                        ; dumb solution but the proffesor asked to use sort and (= ) 
  (let [appear (sort (map suit(frequencies (map rank hand))))] (= appear ["2" "3"])))

(defn two-pairs? [hand]
  (let [suit1 (sort (vals (frequencies (map rank hand))))]
    (or (= suit1 [1 2 2]) (four-of-a-kind? hand))))

(defn straight? [hand]
  (let [straight2? (fn [maxi mini] (== maxi (+ 4 mini)))
        ranked-hand (map rank hand)
        filtered-hand (if (and (== 14 (apply max ranked-hand)) (== 2 (apply min ranked-hand)))
                        (replace {14 1} ranked-hand) ranked-hand)

        maximum (apply max filtered-hand) minimum (apply min filtered-hand)]

    (straight2? maximum minimum)))

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))


(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                   [two-pairs? 2]  [three-of-a-kind? 3]
                   [straight? 4]   [flush? 5]
                   [full-house? 6] [four-of-a-kind? 7]
                   [straight-flush? 8]}
        helper (fn [x] ((first x) hand))]

    (apply max (map second (filter helper checkers)))))
