(ns structured-data)

(defn do-a-thing [x] (let [doublex (+ x x)]
  (Math/pow doublex doublex)))

(defn spiff [v]
  (+ (get v 0) (get v 2))
)

(defn cutify [v]
  (conj v "<3")
  )

(defn spiff-destructuring [v]
  (let [[a b c] v] (+ a c))
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- x2 x1))
  )

(defn height [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (- y2 y1))
  )

(defn square? [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (if (== (- x1 x2) (- y1 y2) )
    true
    false
    ))
  )

(defn area [rectangle]
  (let [[[x1 y1] [x2 y2]] rectangle]
    (* (- x2 x1) (- y2 y1))
  ))

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle] (
    let [[x3 y3] point] (
      if (and (<= x1 x3 x2) (<= y1 y3 y2))
      true
      false
      )
    )
  )
  )

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] outer] (
    let [[[x3 y3] [x4 y4]] inner] (
      if (and (<= x1 x3 x4 x2) (<= y1 y3 y4 y2))
      true
      false
      )
  )
  ))

(defn title-length [book]
  (count (get book :title))
  )

(defn author-count [book]
  (count (get book :authors))
  )

(defn multiple-authors? [book]
  (if (> (author-count book) 1)
  true
  false
  ))

(defn add-author [book new-author]
  (assoc book :authors (conj (get book :authors) new-author))
  )

(defn alive? [author]
  (if (contains? author :death-year)
  false
  true))

(defn element-lengths [collection]
  (map count collection)
  )

(defn second-elements [collection]
  (let [create-seq (fn [x] (get x 1))]
  (map create-seq collection)
  ))

(defn titles [books]
  (map :title books))

(defn monotonic? [a-seq]
  (if (or (apply <= a-seq) (apply >= a-seq))
  true
  false
  ))

(defn stars [n]
  (apply str (repeat n "*")))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
  (disj a-set elem)
  (conj a-set elem)
  ))

(defn contains-duplicates? [a-seq]
  (if (== (count a-seq) (count (set a-seq)))
  false
  true
  ))

(defn old-book->new-book [book]
  (assoc book :authors (set (get book :authors)))
  )

(defn has-author? [book author]
  (if (contains? (get book :authors) author)
  true
  false
  ))

(defn authors [books]
  (let [author-names
         (fn [book] (map :name (:authors book)))]
  (apply clojure.set/union (map :authors books))
  ))

(defn all-author-names [books]
  (let [author-names
       (fn [book] (set(map :name (:authors book))))]
  (apply clojure.set/union (map author-names books))
  ))

(defn author->string [author]
  (let [born (:birth-year author)](
    let [dead (:death-year author)](
      let [name (:name author)]
      (cond
        (boolean dead) (str name " (" born " - " dead ")" )
        (boolean born) (str name " (" born " - )" )
        :else
        (str name)
  )))))

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors)))
  )

(defn book->string [book]
  (apply str (:title book) ", written by " (authors->string (:authors book))
))

(defn count-books [books]
(count (map book->string books))
  )

(defn books->string [books]
  (let [book-count (count-books books)]
    (cond
      (== 0 book-count) "No books."
      (== 1 book-count)
        (apply str (str book-count) " book. "
        (apply str (map book->string books)) ".")
      :else
        (apply str (str book-count) " books. "
        (apply str (interpose ". " (map book->string books))
        ) ".")
  )))

(defn books-by-author [author books]
  (filter (fn [book] (has-author? book author)) books))


  (defn author-by-name [name authors]
    (let [result (filter (fn [author] (= (:name author) name)) authors)]
      (cond
        (empty? result) nil
        :else
          (first result)
          )))

(defn living-authors [authors]
  (filter (fn [author] (alive? author)) authors)
  )

(defn has-a-living-author? [book]
  (cond
    (empty? (filter (fn [author] (alive? author)) (:authors book)))
      false
    :else
      true
  ))

(defn books-by-living-authors [books]
  (filter (fn [book] (has-a-living-author? book)) books)  
  )

; %________%
