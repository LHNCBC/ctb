(ns ctb.keylistexpansion
  (:require [clojure.set :refer [intersection union]]
            [clojure.string :refer [lower-case trim]]
            [skr.tokenization :as tokenization]
            [skr.mwi-utilities :as mwi]
            [umls-tables.core :refer [mrconso-line-record-to-map]]
            [ctb.umls-indexed :refer [lookup] :as umls-indexed]
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
  [indexes cui]
  (mapv #(mwi/normalize-ast-string (:str (mrconso-line-record-to-map %)))
        (lookup (:mrconsocui-index indexes) cui)))

(defn get-umls-candidate-synonyms
  "Get candidate synonyms from UMLS for supplied term."
  [indexes term]
  (let [normterm (mwi/normalize-ast-string term)
        concepts-for-term (map #(:cui (mrconso-line-record-to-map %)) 
                               (lookup (:mrconsostr-index indexes) normterm))]
    (sort (into [] (reduce (fn [newset cui]
                             (union newset (set (terms-for-concept indexes cui))))
                           #{} concepts-for-term)))))

(defn init-lvg
  [lvgdir]
  (when (not (nil? lvgdir))
    (lvg.core/init lvgdir)))

(defn get-lvg-fruitful-candidate-synonyms
  "Get candidate synonyms from LVG Fruitful Variants for supplied
  term."
  [lvg-api term]
  (->> term
       mwi/normalize-ast-string
       (get-fruitful-variants-lex lvg-api)
       (map #(mwi/normalize-ast-string (:target-term %)))))

(defn get-lvg-expansions-candidate-synonyms
  "Get candidate synonyms from LVG acronym expansions for supplied
  term."
  [lvg-api term]
  (->> term
       mwi/normalize-ast-string
       (get-acronym-expansions lvg-api)
       (map #(mwi/normalize-ast-string (:target-term %)))))

(defn get-candidate-synonyms
  "Get candidate synonyms for supplied term.  If LVG is not available
  then just return UMLS candidates for term."
  [lvg-api indexes term]
  (if (nil? lvg-api)
    (get-umls-candidate-synonyms indexes term)
    (conj (intersection
           (set (get-lvg-fruitful-candidate-synonyms lvg-api term))
           (set (get-umls-candidate-synonyms indexes term)))
          term)))

(defn generate-termlists
  "Generate termlists for term."
  ([lvg-api indexes term]
   (generate-termlists lvg-api indexes term get-candidate-synonyms))
  ([lvg-api indexes term synonym-lookup-func]
   (mapv #(synonym-lookup-func lvg-api indexes %)
         (tokenization/tokenize-no-ws term))))

(defn mark-term-expansions-cui-str-sab
  "Mark term expansions using cui-str-sab."
  [indexes term-smap]
  (assoc term-smap
         :term-expansion-lists
         (reduce (fn [newmap term]
                   (let [normterm (mwi/normalize-ast-string term)
                         concept-list (mapv #(select-keys (mrconso-line-record-to-map %) [:cui :str :sab])
                                            (lookup (:mrconsostr-index indexes) normterm))]
                     (if (empty? concept-list)
                       newmap
                       (assoc newmap normterm concept-list))))
                 {} (:term-expansion-lists term-smap))))

(defn mark-term-expansions-with-cuis
  "Mark term expansions using concept unique identifiers (cuis)."
  [indexes term-smap]
  (assoc term-smap
         :term-expansion-lists
         (reduce (fn [newmap term]
                   (let [normterm (mwi/normalize-ast-string term)
                         concept-list (set (mapv #(:cui (mrconso-line-record-to-map %))
                                                 (lookup (:mrconsostr-index indexes)
                                                         normterm)))]
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
  [lvg-api indexes term]
  (let [termlists (generate-termlists lvg-api indexes term)]
    (mark-term-expansions-with-cuis
     indexes
     {:term term
      :term-expansion-lists (generate-term-expansion-lists termlists) }
     )))

(defn termlist-info
  "Given a termlist, determine expansions that occur in knowledge
  source (UMLS) "
  [lvg-api indexes termlist]
  (reduce (fn [newmap term]
            (let [term-smap (expand-term lvg-api indexes term)
                  cuilist (set (mapcat #(second %)
                                       (:term-expansion-lists term-smap)))]
              (assoc newmap
                     (:term term-smap)
                     (assoc 
                      (if (empty? cuilist)
                        {:unfiltered-term-expansion-lists (:unfiltered-term-expansion-lists term-smap)}
                        {:term-expansion-lists (:term-expansion-lists term-smap)})
                      :cuilist cuilist))))
          {} termlist))

(defn expand-term-with-lvg
  "Return list of expanded versions of supplied term using lvg version
   of get-candidate-synonyms with generate-termlist ."
  [lvg-api indexes term]
  (let [termlists (generate-termlists lvg-api indexes term)
        term-expansion-lists (generate-term-expansion-lists termlists)
        ;;(get-candidate-synonyms)
        ]
    (mark-term-expansions-with-cuis
     indexes
     {:term term
      :term-expansion-lists term-expansion-lists
      :unfiltered-term-expansion-lists term-expansion-lists}
     )))

(defn termlist-info-with-lvg
  "Given a termlist, determine expansions that occur in knowledge
  source (UMLS) "
  [lvg-api indexes termlist]
  (reduce (fn [newmap term]
            (let [term-smap (expand-term-with-lvg lvg-api indexes (trim term))
                  cuilist (set (mapcat #(second %)
                                       (:term-expansion-lists term-smap)))]
              (assoc newmap
                     (:term term-smap)
                     (assoc 
                      (if (empty? cuilist)
                        {:unfiltered-term-expansion-lists (:unfiltered-term-expansion-lists term-smap)}
                        {:term-expansion-lists (:term-expansion-lists term-smap)})
                      :cuilist cuilist))))
          {} termlist))
