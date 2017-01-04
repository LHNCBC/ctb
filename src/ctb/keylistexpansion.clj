(ns ctb.keylistexpansion
  (:require [clojure.set :refer [intersection union]]
            [clojure.string :refer [lower-case]]
            [skr.tokenization :as tokenization]
            [umls-tables.core :refer [mrconso-line-record-to-map]]
            [ctb.umls-indexed :refer [*memoized-normalize-ast-string*
                                      *mrconsostr-index*
                                      *mrconsocui-index*
                                      lookup] :as umls-indexed]
            [lvg.core]
            [lvg.lvg :refer [get-acronym-expansions]]
            [lvg.fruitful :refer [get-fruitful-variants-lex]]))

;; # Term Expansion Functions
;; 
;; Functions to expand terms, in particular, abbreviations by
;; decomposing the term and searching a knowledge source, in this case
;; the UMLS, with the components of the terms to create possible
;; expansions of the term.  Then using the candidate term expansions,
;; recomposed, to find existing concepts in the knowledge source with
;; references to the recomposed terms.

(defn gen-two-term-expansion
  "Given two lists in order expand concatenation of all possible
  combinations of one term from each list in order."
  [list0 list1]
  (flatten
   (mapv (fn [term0]
           (mapv (fn [term1]
                   (format "%s %s" term0 term1))
                 list1))
         list0)))

(defn gen-three-term-expansion
  "Given three lists in order expand concatenation of all possible
  combinations of one term from each list in order."
  [list0 list1 list2]
  (flatten
   (mapv (fn [term0]
           (mapv (fn [term1]
                   (mapv (fn [term2]
                           (format "%s %s %s" term0 term1 term2))
                         list2))
                 list1))
         list0)))

(defn gen-four-term-expansion
  "Given four lists in order expand concatenation of all possible
  combinations of one term from each list in order."
  [list0 list1 list2 list3]
  (flatten
   (mapv (fn [term0]
           (mapv (fn [term1]
                   (mapv (fn [term2]
                           (mapv (fn [term3]
                                   (format "%s %s %s %s" term0 term1 term2 term3))
                                 list3))
                         list2))
                 list1))
         list0)))

(defn gen-five-term-expansion
  "Given five lists in order expand concatenation of all possible
  combinations of one term from each list in order."
  [list0 list1 list2 list3 list4] 
  (flatten
   (mapv (fn [term0]
           (mapv (fn [term1]
                   (mapv (fn [term2]
                           (mapv (fn [term3]
                                   (mapv (fn [term4]
                                           (format "%s %s %s %s %s" term0 term1 term2 term3 term4))
                                         list4))
                                 list3))
                         list2))
                 list1))
         list0)))

(defn terms-for-concept
  "List terms for concept identified by cui."
  [cui]
  (mapv #(*memoized-normalize-ast-string* (:str (mrconso-line-record-to-map %)))
        (lookup *mrconsocui-index* cui)))

(defn get-umls-candidate-synonyms
  "Get candidate synonyms from UMLS for supplied term."
  [term]
  (let [normterm (*memoized-normalize-ast-string* term)
        concepts-for-term (map #(:cui (mrconso-line-record-to-map %)) 
                               (lookup *mrconsostr-index* normterm))]
    (sort (into [] (reduce (fn [newset cui]
                             (union newset (set (terms-for-concept cui))))
                           #{} concepts-for-term)))))

(def ^:dynamic *lvg-api* nil)

(defn init-lvg
  [lvgdir]
  (def ^:dynamic *lvg-api* (lvg.core/init lvgdir)))

(defn get-lvg-fruitful-candidate-synonyms
  "Get candidate synonyms from LVG Fruitful Variants for supplied
  term."
  [term]
  (->> term
       *memoized-normalize-ast-string*
       get-fruitful-variants-lex
       (map #(*memoized-normalize-ast-string* (:target %)))))

(defn get-lvg-expansions-candidate-synonyms
  "Get candidate synonyms from LVG acronym expansions for supplied
  term."
  [term]
  (->> term
       *memoized-normalize-ast-string*
       get-acronym-expansions
       (map #(*memoized-normalize-ast-string* (:target %)))))

(defn get-candidate-synonyms
  "Get candidate synonyms for supplied term."
  [term]
  (intersection
   (set (get-lvg-fruitful-candidate-synonyms term))
   (set (get-umls-candidate-synonyms term))))

(defn generate-termlists
  "Generate termlists for term."
  ([term]
   (generate-termlists term get-candidate-synonyms))
  ([term synonym-lookup-func]
   (mapv synonym-lookup-func
         (tokenization/tokenize-no-ws term))))

(defn mark-term-expansions-cui-str-sab
  "Mark term expansions using cui-str-sab."
  [term-smap]
  (assoc term-smap
         :term-expansion-lists
         (reduce (fn [newmap term]
                   (let [normterm (*memoized-normalize-ast-string* term)
                         concept-list (mapv #(select-keys (mrconso-line-record-to-map %) [:cui :str :sab])
                                            (lookup *mrconsostr-index* normterm))]
                     (if (empty? concept-list)
                       newmap
                   (assoc newmap normterm concept-list))))
                 {} (:term-expansion-lists term-smap))))

(defn mark-term-expansions-with-cuis
  "Mark term expansions using concept unique identifiers (cuis)."
  [term-smap]
  (assoc term-smap
         :term-expansion-lists
         (reduce (fn [newmap term]
                   (let [normterm (*memoized-normalize-ast-string* term)
                         concept-list (set (mapv #(:cui (mrconso-line-record-to-map %))
                                                 (lookup *mrconsostr-index* normterm)))]
                     (if (empty? concept-list)
                       newmap
                       (assoc newmap normterm (set (concat (newmap normterm) concept-list))))))
                 {} (:term-expansion-lists term-smap))))

(defn generate-term-expansion-lists
  "Generate expanded termlist based on number of termlists, currently
  only lists of termlists of length between two and three are used."
  [termlists]
  (case (count termlists)
    1 []
    2 (gen-two-term-expansion (first termlists)
                              (second termlists))
    3 (gen-three-term-expansion (first termlists) 
                              (second termlists)
                              (nth termlists 2))
    4 (gen-four-term-expansion (first termlists) 
                               (second termlists)
                               (nth termlists 2)
                               (nth termlists 3))
    (gen-five-term-expansion (first termlists) 
                             (second termlists)
                             (nth termlists 2)
                             (nth termlists 3)
                             (nth termlists 4))
    ))

(defn expand-term
  "Return list of expanded versions of supplied term."
  [term]
  (let [termlists (generate-termlists term)]
        (mark-term-expansions-with-cuis
         {:term term
          :term-expansion-lists (generate-term-expansion-lists termlists) }
         )))

(defn termlist-info
  "Given a termlist, determine expansions that occur in knowledge
  source (UMLS) "
  [termlist]
  (reduce (fn [newmap term]
            (let [term-smap (expand-term term)
                  cuilist (set (apply concat
                                      (mapv #(second %)
                                            (:term-expansion-lists term-smap))))]
              (assoc newmap
                     (:term term-smap)
                     {:cuilist cuilist
                      :term-expansion-lists (if (empty? cuilist)
                                              (:unfiltered-term-expansion-lists term-smap)
                                              (:term-expansion-lists term-smap))})))
          {} termlist))


(defn expand-term-with-lvg
  [term]
  (let [termlists (generate-termlists term)
        term-expansion-lists (generate-term-expansion-lists termlists)]
        (mark-term-expansions-with-cuis
         {:term term
          :term-expansion-lists term-expansion-lists
          :unfiltered-term-expansion-lists term-expansion-lists}
         )))

(defn termlist-info-with-lvg
  "Given a termlist, determine expansions that occur in knowledge
  source (UMLS) "
  [termlist]
  (reduce (fn [newmap term]
            (let [term-smap (expand-term-with-lvg term)
                  cuilist (set (apply concat
                                      (mapv #(second %)
                                            (:term-expansion-lists term-smap))))]
              (assoc newmap
                     (:term term-smap)
                     {:cuilist cuilist
                      :term-expansion-lists (if (empty? cuilist)
                                              (:unfiltered-term-expansion-lists term-smap)
                                              (:term-expansion-lists term-smap))})))
          {} termlist))
