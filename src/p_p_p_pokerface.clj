(ns p-p-p-pokerface)

(defn rank [card]
  (let [[rank _] card
        ranks-map {\T 10 \J 11 \Q 12 \K 13 \A 14}
        digit-char-as-int (fn [rank] (Integer/valueOf (str rank)))
        char-as-int (fn [rank] (get ranks-map rank))]
    (if (Character/isDigit rank)
      (digit-char-as-int rank)
      (char-as-int rank)
      )
    )
  )

(defn suit [card]
  (let [[_ suit] card]
    (str suit)
    )
  )

(defn pair? [hand]
  (let [ranks (map rank hand)
        freqs (frequencies ranks)]
    (if (contains? (set (vals freqs)) 2)
      true
      false
      )
    )
  )

(defn three-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (frequencies ranks)]
    (if (contains? (set (vals freqs)) 3)
      true
      false
      )
    )
  )

(defn four-of-a-kind? [hand]
  (let [ranks (map rank hand)
        freqs (frequencies ranks)]
    (if (contains? (set (vals freqs)) 4)
      true
      false
      )
    )
  )

(defn flush? [hand]
  (let [suits (set (map suit hand))]
    (if (= (count suits) 1)
      true
      false
      )
    )
  )

(defn full-house?
  "2 + 3 = full house"
  [hand]
  (if (and (pair? hand) (three-of-a-kind? hand))
    true
    false
    )
  )

(defn two-pairs? [hand]
  (let [ranks (map rank hand)
        freqs (frequencies ranks)
        sorted-freqs (sort (vals freqs))
        two-pair '(1 2 2)
        four '(1 4)]
    (if (or (= sorted-freqs two-pair) (= sorted-freqs four))
      true
      false
      )
    )
  )

(defn straight? [hand]
  (let [expected-1 '(2 3 4 5 14) ;; when ace is 1
        ranks (sort (map rank hand))
        rank-first (first ranks)
        expected (range rank-first (+ rank-first 5))]
    (if (or (= ranks expected) (= ranks expected-1))
      true
      false
      )
    )
  )

(defn straight-flush? [hand]
  (if (and (straight? hand) (flush? hand))
    true
    false
    )
  )

(defn high-card?
  "All hands have a 'high-card' whose value is 0."
  [hand]
  true)


(defn value [hand]
  (let [checkers #{[high-card? 0]
                   [pair? 1]
                   [two-pairs? 2]
                   [three-of-a-kind? 3]
                   [straight? 4]
                   [flush? 5]
                   [full-house? 6]
                   [four-of-a-kind? 7]
                   [straight-flush? 8]
                   }
;;         create helper function to check hand with a checker:
        check (fn [checker] {(second checker) ((first checker) hand)})
;;         get all possible hand and mark if the exist:
        possible-hands (map check checkers)
;;         create filtering function to return only existing hands
        has-in-hand (fn [has-value] (first (vals has-value)))
;;         filter out the non-existing hands:
        existing-hands (filter has-in-hand possible-hands)
;;         get the values from the calculated pairs:
        values (map (fn [pair] (first (keys pair))) existing-hands)
        ]
    (apply max values)
    )
  )
