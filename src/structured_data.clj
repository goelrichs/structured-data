(ns structured-data)

(defn do-a-thing [x]
  (let [commonexpression (+ x x)]
    (Math/pow commonexpression commonexpression)));; => #'structured-data/do-a-thing
(do-a-thing 2);; => 256.0
(do-a-thing 3);; => 46656.0
(do-a-thing 1);; => 4.0

(defn spiff [v]
  (+ (get v 0) (get v 2)));; => #'structured-data/spiff
;(+ 3 nil) throws an error
(get [1 2 3] 0);; => 1
(spiff [1 2 3]);; => 4
(spiff [1 2 3 4 5 6]);; => 4
;;(spiff [1 2]) error as expected
;;(spiff [])

(defn cutify [v]
  (conj v "<3"));; => #'structured-data/cutify
(cutify []);; => ["<3"]
(cutify [1 2 3]);; => [1 2 3 "<3"]
(cutify ["a" "b"]);; => ["a" "b" "<3"]

(defn spiff-destructuring [v]
  (let [[x y z] v]
    (+ x z)));; => #'structured-data/spiff-destructuring
(spiff-destructuring [1 2 3]);; => 4

(defn point [x y]
  [x y]);; => #'structured-data/point
(point 1 2);; => [1 2]

(defn rectangle [bottom-left top-right]
  [bottom-left top-right]);; => #'structured-data/rectangle

(defn width [rectangle]
  (let [[[x y] [a b]] rectangle]
    (- a x)));; => #'structured-data/width
(width (rectangle [1 1] [5 1]));; => 4
(width (rectangle [1 1] [1 5]));; => 0
(width (rectangle [3 1] [10 4]));; => 7

(defn height [rectangle]
  (let [[[x y] [a b]] rectangle]
    (- b y)));; => #'structured-data/height

(defn square? [rectangle]
  (= (height rectangle) (width rectangle)));; => #'structured-data/square?
(square? (rectangle [1 1] [5 1]));; => false
(square? (rectangle [1 1] [2 2]));; => true
(square? (rectangle [1 1] [2 3]));; => false
(square? (rectangle [1 1] [1 1]));; => true
(square? (rectangle [3 2] [1 0]));; => true
(square? (rectangle [3 2] [1 1]));; => false

(defn area [rectangle]
  (* (width rectangle) (height rectangle)));; => #'structured-data/area
(area (rectangle [1 1] [5 1]));; => 0
(area (rectangle [0 0] [1 1])) ;; => 1
(area (rectangle [0 0] [4 3]));; => 12
(area (rectangle [3 1] [10 4]));; => 21

(defn contains-point? [rectangle point]
  (let [[[x1 y1] [x2 y2]] rectangle
        [a b] point]
    (and (>= y2 b) (>= x2 a) (>= b y1) (>= a x1))
    ));; => #'structured-data/contains-point?
(contains-point? (rectangle [0 0] [2 2]) (point 1 1));; => true
(contains-point? (rectangle [0 0] [2 2]) (point 2 1));; => true
(contains-point? (rectangle [0 0] [2 2]) (point -3 1));; => false
(contains-point? (rectangle [0 0] [2 2]) (point 1 3));; => false
(contains-point? (rectangle [1 1] [2 2]) (point 1 1));; => true
(contains-point? (rectangle [1 1] [1 1]) (point 1 1));; => true

(defn contains-rectangle? [outer inner]
  (let [[bottom-left top-right] inner]
    (and (contains-point? outer bottom-left) (contains-point? outer top-right))
    ));; => #'structured-data/contains-rectangle?
(contains-rectangle? (rectangle [0 0] [3 3]) (rectangle [1 1] [2 2]));; => true
(contains-rectangle? (rectangle [0 0] [2 2]) (rectangle [1 1] [3 3]));; => false
(contains-rectangle? (rectangle [0 0] [1 1]) (rectangle [0 0] [1 1]));; => true
(contains-rectangle? (rectangle [0 0] [1 1]) (rectangle [1 1] [2 2]));; => false

(comment 
  (def china {:name "China Miéville", :birth-year 1972});; => #'structured-data/china
  (def octavia {:name "Octavia E. Butler"
                :birth-year 1947
                :death-year 2006});; => #'structured-data/octavia
  (def friedman {:name "Daniel Friedman" :birth-year 1944});; => #'structured-data/friedman
  (def felleisen {:name "Matthias Felleisen"});; => #'structured-data/felleisen

  (def cities {:title "The City and the City" :authors [china]});; => #'structured-data/cities
  (def wild-seed {:title "Wild Seed", :authors [octavia]});; => #'structured-data/wild-seed
  (def embassytown {:title "Embassytown", :authors [china]});; => #'structured-data/embassytown
  (def little-schemer {:title "The Little Schemer"
                       :authors [friedman, felleisen]}));; => #'structured-data/little-schemer

(defn title-length [book]
  (count (:title book)));; => #'structured-data/title-length
;;(title-length cities);; => 21
;;(title-length wild-seed);; => 9
;;(title-length little-schemer);; => 18

(defn author-count [book]
  (count (:authors book)));; => #'structured-data/author-count
;;(author-count cities);; => 1
;;(author-count wild-seed);; => 1
;;(author-count little-schemer);; => 2

(defn multiple-authors? [book]
  (> (author-count book) 1));; => #'structured-data/multiple-authors?
;;(multiple-authors? cities);; => false
;;(multiple-authors? wild-seed);; => false
;;(multiple-authors? little-schemer);; => true

(defn add-author [book new-author]
  (assoc book :authors (conj (:authors book) new-author)
    ));; => #'structured-data/add-author
;;(add-author little-schemer {:name "Gerald J. Sussman"});; => {:title "The Little Schemer",
;;     :authors
;;     [{:name "Daniel Friedman", :birth-year 1944}
;;      {:name "Matthias Felleisen"}
;;      {:name "Gerald J. Sussman"}]}
;;(add-author {:authors [{:name "Johana"}]} {:name "Jani"});; => {:authors [{:name "Johana"} {:name "Jani"}]}

;; want to "use let to avoid pain" so am trying the above function again
;; I don't think making up new local variables made this any better, but it does work
(defn add-author2 [book new-author]
  (let [vect-of-authors (:authors book); calling the keywork :authors as a function returns a vect
        new-vect-of-authors (conj vect-of-authors new-author)]; conj ads the new-auth map at the end of the vector
    (assoc book :authors new-vect-of-authors)
    ));; => #'structured-data/add-author2
;;(add-author2 little-schemer {:name "Gerald J. Sussman"});; => {:title "The Little Schemer",
;;     :authors
;;     [{:name "Daniel Friedman", :birth-year 1944}
;;      {:name "Matthias Felleisen"}
;;      {:name "Gerald J. Sussman"}]}
;;(add-author2 {:authors [{:name "Johana"}]} {:name "Jani"});; => {:authors [{:name "Johana"} {:name "Jani"}]}

;;(:name china);; => "China Miéville"
;;(:authors little-schemer);; => [{:name "Daniel Friedman", :birth-year 1944}
;;     {:name "Matthias Felleisen"}]

(defn alive? [author]
  (not (contains? author :death-year)));; => #'structured-data/alive?
;;(alive? china);; => true
;;(alive? octavia);; => false
;;(set octavia);; => #{[:birth-year 1947] [:death-year 2006] [:name "Octavia E. Butler"]}

(defn element-lengths [collection]
  (map count collection));; => #'structured-data/element-lengths
(element-lengths ["foo" "bar" "" "quux"]);; => (3 3 0 4)
(element-lengths ["x" [:a :b :c] {:y 42}]);; => (1 3 1)

(defn second-elements [collection]
  (map second collection));; => #'structured-data/second-elements
(second-elements [[1 2] [2 3] [3 4]]);; => (2 3 4)
(second-elements [[1 2 3 4] [1] ["a" "s" "d" "f"]]);; => (2 nil "s")

(comment 
  (def china {:name "China Miéville", :birth-year 1972});; => #'structured-data/china
  (def octavia {:name "Octavia E. Butler"
                :birth-year 1947
                :death-year 2006});; => #'structured-data/octavia
  (def friedman {:name "Daniel Friedman" :birth-year 1944});; => #'structured-data/friedman
  (def felleisen {:name "Matthias Felleisen"});; => #'structured-data/felleisen

  (def cities {:title "The City and the City" :authors [china]});; => #'structured-data/cities
  (def wild-seed {:title "Wild Seed", :authors [octavia]});; => #'structured-data/wild-seed
  (def embassytown {:title "Embassytown", :authors [china]});; => #'structured-data/embassytown
  (def little-schemer {:title "The Little Schemer"
                       :authors [friedman, felleisen]});; => #'structured-data/little-schemer

  (def books [cities, wild-seed, embassytown, little-schemer]));; => #'structured-data/books

(defn titles [books]
  (map :title books));; => #'structured-data/titles
;;(titles [cities]);; => ("The City and the City")
;;(titles books);; => ("The City and the City"
;;     "Wild Seed"
;;     "Embassytown"
;;     "The Little Schemer")

(defn monotonic? [a-seq]
  (or (apply <= a-seq) (apply >= a-seq)));; => #'structured-data/monotonic?
(monotonic? [1 2 3]);; => true
(monotonic? [0 1 10 11]);; => true
(monotonic? [3 2 0 -3]);; => true
(monotonic? [3 2 2]);; => true
(monotonic? [1 2 1 0]);; => false

(defn stars [n]
  (apply str (repeat n "*")));; => #'structured-data/stars
(stars 1);; => "*"
(stars 5);; => "*****"
(stars 7);; => "*******"

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)));; => #'structured-data/toggle
(toggle #{:a :b :c} :d);; => #{:c :b :d :a}
(toggle #{:a :b :c} :a);; => #{:c :b}

(defn contains-duplicates? [a-seq]
  (not (= (count a-seq) (count (set a-seq)))
            ));; => #'structured-data/contains-duplicates?
(contains-duplicates? [1 1 2 3 -40]);; => true
(contains-duplicates? [1 2 3 "a" "a"]);; => true
(set [1 1 2 3 -40]);; => #{1 3 2 -40}
(seq #{1 3 2 -40});; => (1 3 2 -40)
(seq [1 1 2 3 -40]);; => (1 1 2 3 -40)
(contains-duplicates? [1 2 3 -40]);; => false
(contains-duplicates? [1 2 3 "a" "a"]);; => true
(= (set (seq [1 2 3 4])) (set [1 2 3 4]));; => true
(seq [1 2 3 4]);; => (1 2 3 4)
(seq (set [1 2 3 4]));; => (1 4 3 2)

(defn old-book->new-book [book]
  (assoc book :authors (set (:authors book))));; => #'structured-data/old-book->new-book
;;(old-book->new-book {:title "Wild Seed", :authors [octavia]});; => {:title "Wild Seed",
;;     :authors
;;     #{{:name "Octavia E. Butler", :birth-year 1947, :death-year 2006}}}

;;(def china {:name "China Miéville", :birth-year 1972})
;;(def octavia {:name "Octavia E. Butler" :birth-year 1947 :death-year 2006})
;;(def friedman {:name "Daniel Friedman" :birth-year 1944})
;;(def felleisen {:name "Matthias Felleisen"})

;;(def cities {:title "The City and the City" :authors #{china}})
;;(def wild-seed {:title "Wild Seed", :authors #{octavia}})
;;(def embassytown {:title "Embassytown", :authors #{china}})
;;(def little-schemer {:title "The Little Schemer" :authors #{friedman, felleisen}})
;;(def books [cities, wild-seed, embassytown, little-schemer])

(defn has-author? [book author]
  (contains? (:authors book) author));; => #'structured-data/has-author?
;;(has-author? cities china);; => true          
;;(has-author? cities felleisen);; => false
;;(has-author? little-schemer felleisen);; => true
;;(has-author? little-schemer friedman);; => true
;;(has-author? little-schemer octavia);; => false

;;#{octavia,china};; => #{{:name "China Miéville", :birth-year 1972}
;;      {:name "Octavia E. Butler", :birth-year 1947, :death-year 2006}}
;;#{china,octavia};; => #{{:name "China Miéville", :birth-year 1972}
;;      {:name "Octavia E. Butler", :birth-year 1947, :death-year 2006}}

(defn authors [books]
  (apply clojure.set/union (map :authors books)));; => #'structured-data/authors
;;(authors [cities, wild-seed]);; => #{{:name "China Miéville", :birth-year 1972}
;;      {:name "Octavia E. Butler", :birth-year 1947, :death-year 2006}}
;;(authors [cities, wild-seed, embassytown]);; => #{{:name "China Miéville", :birth-year 1972}
;;      {:name "Octavia E. Butler", :birth-year 1947, :death-year 2006}}
;;(authors [little-schemer, cities]);; => #{{:name "China Miéville", :birth-year 1972}
;;      {:name "Daniel Friedman", :birth-year 1944}
;;      {:name "Matthias Felleisen"}}
(defn all-author-names [books]
  (set (map :name (authors books))));; => #'structured-data/all-author-names
;;(all-author-names books);; => #{"Matthias Felleisen" "China Miéville" "Daniel Friedman"
;;      "Octavia E. Butler"}
;;(all-author-names [cities, wild-seed]);; => #{"China Miéville" "Octavia E. Butler"}
;;(all-author-names []);; => #{}

(apply clojure.set/union [#{1 2} #{5} #{7 8}]);; => #{7 1 2 5 8}
;;(apply clojure.set/union (#{1 2} #{5} #{7 8}))  throws an error...can't be a seq, must be vect

(defn author->string [author]
  (let [name (:name author)
        years (str " (" (:birth-year author) " - " (:death-year author) ")")]
    (if (contains? author :birth-year)
      (str name years)
      (str name)
      )));; => #'structured-data/author->string
;;(author->string felleisen);; => "Matthias Felleisen"
;;(author->string friedman);; => "Daniel Friedman (1944 - )"
;;(author->string octavia);; => "Octavia E. Butler (1947 - 2006)"

(defn authors->string [authors]
  (apply str (interpose ", " (map author->string authors))));; => #'structured-data/authors->string
;;(:authors little-schemer);; => #{{:name "Daniel Friedman", :birth-year 1944}
;;      {:name "Matthias Felleisen"}}
;;(authors->string (:authors little-schemer));; => "Daniel Friedman (1944 - ), Matthias Felleisen"
;;(authors->string #{octavia});; => "Octavia E. Butler (1947 - 2006)"          
;;(authors->string #{});; => ""                 
;;(authors->string #{octavia, friedman});; => "Daniel Friedman (1944 - ), Octavia E. Butler (1947 - 2006)"

(defn book->string [book]
  (str (:title book) ", written by " (authors->string (:authors book))));; => #'structured-data/book->string
;;(book->string wild-seed);; => "Wild Seed, written by Octavia E. Butler (1947 - 2006)"
;;(book->string little-schemer);; => "The Little Schemer, written by Daniel Friedman (1944 - ), Matthias Felleisen"
;;(book->string cities);; => "The City and the City, written by China Miéville (1972 - )"

(defn books->string [books]
  (let [n (count books)]
    (cond
      (= 0 n) "No books."
      (= 1 n) (str "1 book. " (book->string (first books)) ".")
      (> n 1) (str n " books. " (apply str (interpose ". " (map book->string books))) ".")
      )));; => #'structured-data/books->string
;;(books->string []);; => "No books."
;;(books->string [cities]);; => "1 book. The City and the City, written by China Miéville (1972 - )"
;;(books->string [little-schemer, cities, wild-seed]);; => "3 books. The Little Schemer, written by Daniel Friedman (1944 - ), Matthias Felleisen. The City and the City, written by China Miéville (1972 - ). Wild Seed, written by Octavia E. Butler (1947 - 2006)."

(defn books-by-author [author books]
  (filter (fn [x] (has-author? x author)) books));; => #'structured-data/books-by-author
(defn has-author? [book author]
  (contains? (:authors book) author))
;;china;; => {:name "China Miéville", :birth-year 1972}
;;cities;; => {:title "The City and the City",
;;     :authors #{{:name "China Miéville", :birth-year 1972}}}
;;books;; => [{:title "The City and the City",
;;      :authors #{{:name "China Miéville", :birth-year 1972}}}
;;     {:title "Wild Seed",
;;      :authors
;;      #{{:name "Octavia E. Butler", :birth-year 1947, :death-year 2006}}}
;;     {:title "Embassytown",
;;      :authors #{{:name "China Miéville", :birth-year 1972}}}
;;     {:title "The Little Schemer",
;;      :authors
;;      #{{:name "Daniel Friedman", :birth-year 1944}
;;        {:name "Matthias Felleisen"}}}]
;;(def books [cities, wild-seed, embassytown, little-schemer])
;;(def cities {:title "The City and the City" :authors #{china}})
;;(def china {:name "China Miéville", :birth-year 1972})

;;(has-author? cities china);; => true
;;(has-author? embassytown china);; => true
;;(books-by-author china books);; => ({:title "The City and the City",
;;      :authors #{{:name "China Miéville", :birth-year 1972}}}
;;     {:title "Embassytown",
;;      :authors #{{:name "China Miéville", :birth-year 1972}}})  
;;(books-by-author octavia books);; => ({:title "Wild Seed",
;;      :authors
;;      #{{:name "Octavia E. Butler", :birth-year 1947, :death-year 2006}}})
;;(def authors #{china, felleisen, octavia, friedman});; => #'structured-data/authors

(defn author-by-name [name authors] 
  (first (filter (fn [x] (= name (:name x))) authors)));; => #'structured-data/author-by-name
;;(author-by-name "Octavia E. Butler" authors);; => {:name "Octavia E. Butler", :birth-year 1947, :death-year 2006}
;;(author-by-name "Octavia E. Butler" #{felleisen, friedman});; => nil
;;(author-by-name "China Miéville" authors);; => {:name "China Miéville", :birth-year 1972}
;;(author-by-name "Goerge R. R. Martin" authors);; => nil

(defn living-authors [authors]
  (filter (fn [x] (alive? x)) authors))
(defn living-authors2 [authors]
  (filter alive? authors));; => #'structured-data/living-authors2
;;(living-authors2 authors);; => ({:name "China Miéville", :birth-year 1972}
;;     {:name "Daniel Friedman", :birth-year 1944}
;;     {:name "Matthias Felleisen"})
;;(living-authors authors);; => ({:name "China Miéville", :birth-year 1972}
;;     {:name "Daniel Friedman", :birth-year 1944}
;;     {:name "Matthias Felleisen"})            
;;(living-authors #{octavia});; => ()         
;;(living-authors #{china, felleisen});; => ({:name "China Miéville", :birth-year 1972}
;;     {:name "Matthias Felleisen"})

;;rember the alive? function?...helps with above filter function
(defn alive? [author]
  (not (contains? author :death-year)))

;;(def jrrtolkien {:name "J. R. R. Tolkien" :birth-year 1892 :death-year 1973})
;;(def christopher {:name "Christopher Tolkien" :birth-year 1924})
;;(def kay {:name "Guy Gavriel Kay" :birth-year 1954})


;;(def silmarillion {:title "Silmarillion" :authors #{jrrtolkien, christopher, kay}})
;;(def dick {:name "Philip K. Dick", :birth-year 1928, :death-year 1982})
;;(def zelazny {:name "Roger Zelazny", :birth-year 1937, :death-year 1995})

;;(def deus-irae {:title "Deus Irae", :authors #{dick, zelazny}})

(defn has-a-living-author? [book]
  (not (empty? (living-authors (:authors book)))));; => #'structured-data/has-a-living-author?
;;(has-a-living-author? wild-seed);; => false     
;;(has-a-living-author? silmarillion);; => true  
;;(has-a-living-author? little-schemer);; => true
;;(has-a-living-author? cities);; => true     
;;(has-a-living-author? deus-irae);; => false  

(defn books-by-living-authors [books]
 (filter has-a-living-author? books));; => #'structured-data/books-by-living-authors
;;(books-by-living-authors books)
;; => ({:title "The City and the City",
;;      :authors #{{:name "China Miéville", :birth-year 1972}}}
;;     {:title "Embassytown",
;;      :authors #{{:name "China Miéville", :birth-year 1972}}}
;;     {:title "The Little Schemer",
;;      :authors
;;      #{{:name "Daniel Friedman", :birth-year 1944}
;;        {:name "Matthias Felleisen"}}})
;;(books-by-living-authors (concat books [deus-irae, silmarillion]))
;; => ({:title "The City and the City",
;;      :authors #{{:name "China Miéville", :birth-year 1972}}}
;;     {:title "Embassytown",
;;      :authors #{{:name "China Miéville", :birth-year 1972}}}
;;     {:title "The Little Schemer",
;;      :authors
;;      #{{:name "Daniel Friedman", :birth-year 1944}
;;        {:name "Matthias Felleisen"}}}
;;     {:title "Silmarillion",
;;      :authors
;;      #{{:name "J. R. R. Tolkien", :birth-year 1892, :death-year 1973}
;;        {:name "Christopher Tolkien", :birth-year 1924}
;;        {:name "Guy Gavriel Kay", :birth-year 1954}}})

                                        ; %________%
