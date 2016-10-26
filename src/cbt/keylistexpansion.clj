(ns cbt.keylistexpansion
  (:require [clojure.set :refer [union]]
            [clojure.string :refer [lower-case]]
            [skr.tokenization :as tokenization]
            [umls-tables.core :refer [mrconso-line-record-to-map]]
            [cbt.umls-indexed :refer [*memoized-normalize-ast-string*
                                            *mrconsostr-index*
                                            *mrconsocui-index*
                                            lookup]
             :as umls-indexed]))

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
  "Given two lists in order expand concatenation of all possible
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

(defn terms-for-concept
  "List terms for concept identified by cui."
  [cui]
  (mapv #(:str (mrconso-line-record-to-map %))
        (lookup *mrconsocui-index* cui)))

(defn get-candidate-synonyms
  "Get candidate synonyms for supplied term."
  [term]
  (let [normterm (*memoized-normalize-ast-string* term)
        concepts-for-term (map #(:cui (mrconso-line-record-to-map %)) 
                               (lookup *mrconsostr-index* normterm))]
    (sort (into [] (reduce (fn [newset cui]
                             (union newset (set (terms-for-concept cui))))
                           #{} concepts-for-term)))))

(defn generate-termlists
  "Generate termlists for term."
  [term]
  (mapv get-candidate-synonyms
        (tokenization/tokenize-no-ws term)))\

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

(defn expand-term
  "Return list of expanded versions of supplied term."
  [term]
  (let [termlists (generate-termlists term)]
        (mark-term-expansions-with-cuis
         {:term term
          :term-expansion-lists (case (count termlists)
                                  1 []
                                  2 (gen-two-term-expansion (first termlists)
                                                            (second termlists))
                                  (gen-three-term-expansion (first termlists)
                                                            (second termlists)
                                                            (nth termlists 2))) }
         )))

(defn expand-termlist
  "Given a termlist, determine expansions that occur in knowledge
  source (UMLS) "
  [termlist]
  (reduce (fn [newmap term]
            (let [term-smap (expand-term term)]
              (assoc newmap
                     (:term term-smap)
                     (set (apply concat
                                 (mapv #(second %)
                                       (:term-expansion-lists term-smap)))))))
          {} termlist))
