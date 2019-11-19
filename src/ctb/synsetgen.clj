(ns ctb.synsetgen
  (:require [clojure.java.io :as io]
            [clojure.string :refer [join split lower-case]]
            [clojure.set :refer [union]]
            [clojure.tools.logging :as log]
            [skr.rrf-mrconso-utils :as rrf-mrconso]
            [skr.rrf-mrsty-utils :as rrf-mrsty]
            [skr.mwi-utilities :as mwi]
            [umls-tables.core :refer [mrconso-line-record-to-map
                                      mrsat-line-record-to-map
                                      mrsty-line-record-to-map]]
            [ctb.umls-indexed :refer [generate-term-conceptid-map
                                      get-preferred-name
                                      get-mrsty-records]])
  (:import (java.io Writer)
           (gov.nih.nlm.nls.nlp.nlsstrings NLSStrings)))

;; # Synonym Set Generation (synsetgen)
;; 

(defonce memoized-normalize-ast-string (memoize mwi/normalize-ast-string))
(defonce default-sab (atom "custom"))
(defonce hide-vocab-sources (atom false))

(defn set-hide-vocab-sources!
  [boolvalue]
  (reset! hide-vocab-sources boolvalue))

(defn term-cui-termset-map-to-cui-termset-map
  "Convert term -> cui -> termset map to cui -> termset map"
  [term-cui-termset-map]
  (reduce (fn [newmap [term cui-termset-map]]
            (reduce (fn [submap [cui termset]]
                      (assoc submap cui (union (submap cui) (cui-termset-map cui))))
                    newmap cui-termset-map))
          {} term-cui-termset-map))

(defn blank-source-information
  "Blank src, saui, scui, sdui, and sab fields in UMLS derived MRCONSO
  records."
  [record]
  (if (= (:sab record) "custom")
    record
    (assoc 
     (dissoc record [:src :saui :scui :sdui :sab :code])
     :src ""
     :saui ""
     :scui ""
     :sdui ""
     :sab ""
     :code "") ))

(defn transform-record
  [record]
  (if @hide-vocab-sources
    (blank-source-information record)
    record) )
  

(defn mrconso-from-cui-concept-map-write
    "Write MRCONSO records in cui-concept-map to writer"
  ([^Writer wtr cui-concept-map]
  (dorun
   (map (fn [cui]
          (dorun
           (map (fn [record]
                  (.write wtr
                          (format "%s\n"
                                  (rrf-mrconso/mrconso-map-to-line-record
                                   (blank-source-information record)))))
                (cui-concept-map cui))))
        (sort (keys cui-concept-map)))))
  ([^Writer wtr cui-concept-map filtered-synset]
   (let [cui-termset (term-cui-termset-map-to-cui-termset-map filtered-synset)]
       (dorun
        (map (fn [cui]
               (dorun
                (map (fn [record]
                       (when (contains? (set (cui-termset cui)) (:str record))
                         (.write wtr
                                 (format "%s\n"
                                         (rrf-mrconso/mrconso-map-to-line-record
                                          (blank-source-information record))))))
                     (cui-concept-map cui))))
             (sort (keys cui-concept-map)))))))

(defn mrconso-records-from-cui-concept-map
    "Write MRCONSO records in cui-concept-map to writer"
  ([cui-concept-map]
   (mapcat (fn [cui]
             (map (fn [record]
                    (format "%s\n"
                            (rrf-mrconso/mrconso-map-to-line-record
                             (blank-source-information record))))
                  (cui-concept-map cui)))
           (sort (keys cui-concept-map))))
  ([cui-concept-map filtered-synset]
   (let [cui-termset (term-cui-termset-map-to-cui-termset-map filtered-synset)]
     (mapcat (fn [cui]
               (map (fn [record]
                      (if (contains? (set (cui-termset cui)) (:str record))
                        (format "%s\n"
                                (rrf-mrconso/mrconso-map-to-line-record
                                 (blank-source-information record)))
                        ""))
                    (cui-concept-map cui)))
             (sort (keys cui-concept-map))))))

(defn write-mrconso-from-cui-concept-map
  "Write MRCONSO records in cui-concept-map to file"
  ([filename cui-concept-map]
   (with-open [wtr (io/writer filename :encoding "UTF-8")]
     (dorun
      (map (fn [cui]
             (dorun
              (map (fn [record]
                     (.write wtr (format "%s\n"(rrf-mrconso/mrconso-map-to-line-record (transform-record record)))))
                   (cui-concept-map cui))))
           (sort (keys cui-concept-map))))))
  ([filename cui-concept-map filtered-synset]
   (let [cui-termset (term-cui-termset-map-to-cui-termset-map filtered-synset)]
     (with-open [wtr (io/writer filename :encoding "UTF-8")]
       (dorun
        (map (fn [cui]
               (dorun
                (map (fn [record]
                       (when (contains? (set (cui-termset cui)) (:str record))
                         (.write wtr (format "%s\n"(rrf-mrconso/mrconso-map-to-line-record (transform-record record))))))
                     (cui-concept-map cui))))
             (sort (keys cui-concept-map))))))))


  
(defn generate-term-cui-termset-map
  "Generate map of term -> cui -> termset from termlist, term-conceptid-map,
  and cui-concept-map."
  [termlist term-conceptid-map cui-concept-map]
  (reduce (fn [term-map term]
            (let [nmterm (memoized-normalize-ast-string term)
                  cuiset (term-conceptid-map nmterm)]
              (assoc term-map term
                     (reduce (fn [synset-map cui]
                               (assoc synset-map cui
                                      (set
                                       (mapv #(:str %)
                                             (cui-concept-map cui)))))
                             {} cuiset) )))
          {} termlist))

;;
;; The function _inc-cui-index_ should be replaced by functional code that generates
;; a set of synthetic mrconso records using collected strings that
;; don't occur in knowledge source.
;;

(def cui-index (atom 1))

(defn inc-cui-index
  "increment global cui index"
  []
  (swap! cui-index inc)
  @cui-index)

(defn syntactically-simple?
  "Is string syntactically-simple and contains no NOS or NEC or
  multiple meaning designators.

  Not the same as the function isSyntacticallySimple which determines
  the number of Minimal Syntactic Units (noMSUs) present in a string
  where the number of Minimal Syntactic Units is below some
  pre-determined threshold.

"  [term]
  (and
   (= (NLSStrings/eliminateNosString term) (lower-case term))
   (= (NLSStrings/eliminateMultipleMeaningDesignatorString term) term)
   ;; (not (NLSStrings/containsPrepOrConj term))
   (not (NLSStrings/abgn_form term))
  ))

(defn generate-term-cui-conceptinfo-map
  "Generate map of term -> synsets from termlist, term-conceptid-map,
  and cui-concept-map."
  [termlist term-conceptid-map cui-concept-map unmapped-term-expanded-info-map]
  (reduce (fn [term-map term]
            (let [nmterm (memoized-normalize-ast-string term)
                  cuiset (term-conceptid-map nmterm)]
              (if (> (count cuiset) 0)
                (assoc term-map term
                       (reduce (fn [synset-map cui]
                                 (assoc synset-map
                                        cui {:preferred-name (get-preferred-name cui)
                                             :termset (set (conj (map #(:str %)
                                                                       (cui-concept-map cui))
                                                                 term))
                                             :suppress-set (set (map #(:str %)
                                                                     (filter #(not (syntactically-simple? (:str %)))
                                                                             (cui-concept-map cui))))}))
                               {} cuiset) )
                (let [termset (if (contains? unmapped-term-expanded-info-map (memoized-normalize-ast-string term))
                                (set
                                 (conj 
                                  (:unfiltered-term-expansion-lists
                                   (unmapped-term-expanded-info-map (memoized-normalize-ast-string term)))
                                  term))
                                #{ term })]
                  (assoc term-map term {(format "D%07d" (inc-cui-index))
                                        {:preferred-name term
                                         :termset termset
                                         :suppress-set
                                         (disj termset
                                               (memoized-normalize-ast-string term))}})) )))
          {} termlist))

(defn generate-synthetic-mrconso-record
  "Make a synthetic mrconso record"
  ([cui lui sui aui sab tty code term]
   (hash-map
    :cui cui
    :lat "ENG"
    :ts "P"
    :lui lui
    :stt "PF"
    :sui sui
    :ispref "Y"
    :aui aui
    :saui ""
    :sdui ""
    :sab sab
    :tty tty
    :code code
    :str term
    :srl 0
    :suppress "N"
    :cvf 0))
  ([term cui]
   (generate-synthetic-mrconso-record
    cui
    "" ;; (get-lui term)
    "" ;; (get-sui term)
    "" ;; (get-aui term cui sab)
    @default-sab
    ""
    ""
    term))
  ([term]
   (generate-synthetic-mrconso-record
    (format "D%07d" (inc-cui-index))
    "" ;; (get-lui term)
    "" ;; (get-sui term)
    "" ;; (get-aui term cui sab)
    "custom"
    ""
    ""
    term)))

(defn generate-augmented-mrconso-records
  "Generate augmented mrconso record list from term -> cui -> termset
  map and cui -> concept record list map."
  [term-synset-map cui-concept-map]
  (flatten
   (mapv (fn [[term cui-termset-map]]
           (if (empty? cui-termset-map)
             (generate-synthetic-mrconso-record term)
             (mapv (fn [[cui termset]]
                     (if (contains? (set (map memoized-normalize-ast-string termset))
                                    (memoized-normalize-ast-string term))
                       (cui-concept-map cui)
                       (conj (cui-concept-map cui)
                             (generate-synthetic-mrconso-record term))))
                   cui-termset-map)))
         term-synset-map)))


(defn generate-augmented-mrconso-records-map
  "Generate map of augmented mrconso records by term."
  [term-synset-map cui-concept-map]
   (reduce (fn [newmap [term cui-termset-map]]
             (if (empty? cui-termset-map)
               (let [record (generate-synthetic-mrconso-record term)]
                 (assoc newmap term {(:cui record) (vector record)}))
               (assoc newmap term
                      (reduce (fn [cmap [cui termset]]
                                (if (contains? (set (map memoized-normalize-ast-string termset))
                                               (memoized-normalize-ast-string term))
                                  (assoc cmap cui (cui-concept-map cui))
                                  (assoc cmap cui (conj (cui-concept-map cui)
                                                        (generate-synthetic-mrconso-record term)))))
                              {} cui-termset-map))))
           {} term-synset-map))

(defn serialize-augmented-mrconso-records-map
  "Serialize augmented MRCONSO records to file."
  [filename augmented-mrconso-records-map]
  (with-open [wtr (io/writer filename :encoding "UTF-8")]
    (dorun
     (map (fn [[term cui-records-map]]
            (dorun
             (map (fn [[cui records-list]]
                    (dorun
                     (map (fn [record]
                            (.write wtr (str (join "|"(mapv #(record %)
                                                            rrf-mrconso/*rrf-mrconso-label-order*))
                                             "\n")))
                          records-list)))
                  cui-records-map)))
          augmented-mrconso-records-map))))

(defn list-unmapped-terms
  "List any term not mapped to any concepts."
  [term-synset-map]
  (mapv first
        (filterv #(empty? (second %)) term-synset-map)))

(defn load-mrsty
  "Load MRSTY (concept -> semantic type) file. "
  [mrstyfn]
  (filter #(= (:lat %) "ENG")
          (mapv rrf-mrsty/mrsty-line-record-to-map
                (rrf-mrsty/load-mrsty mrstyfn))))

(defn generate-custom-mrfile
  "Generate custom UMLS-style MR file containing only concepts in
  cuilist."
  [src-mrfilefn cuilist dst-mrfilefn] 
  (let [cuiset (set cuilist)]
    (with-open [rdr (io/reader src-mrfilefn)]
      (with-open [wtr (io/writer dst-mrfilefn :encoding "UTF-8")]
        (dorun 
         (map (fn [line]
                (let [fields (split line #"\|")]
                  (if (contains? cuiset (first fields))
                    (.write wtr (format "%s\n" line)))))
              (line-seq rdr)))))))

(defn write-custom-mrfile
  "Write UMLS-like MRxxx file to dst-mrfilefn using records with
  fieldlist as guide."
  [recordlist fieldlist dst-mrfilefn]
  (with-open [wtr (io/writer dst-mrfilefn :encoding "UTF-8")]
    (dorun 
     (map (fn [rrfrecord]
            (.write wtr (join "|" (mapv #(get rrfrecord %)
                                        fieldlist)))
            (.write wtr "\n"))
          (sort-by :cui recordlist)))))

(defn generate-custom-mrrank
  [src-mrrankfn custom-sabset]
  (with-open [rdr (io/reader src-mrrankfn)]
    ;; to be implemented
 ))

(defn generate-custom-mrsab
  "Generate a custom version of MRSAB using MRSAB from MetaMorphoSys
  retaining only cuis in cuilist."
  [src-mrsabfn cuilist dst-mrsabfn] 
   (generate-custom-mrfile src-mrsabfn cuilist dst-mrsabfn))

(defn generate-custom-mrsat
  "Generate a custom version of MRSAT using MRSAT from MetaMorphoSys
  retaining only cuis in cuilist."
  [src-mrsatfn cuilist dst-mrsatfn] 
  (generate-custom-mrfile src-mrsatfn cuilist dst-mrsatfn))

(defn gen-mrsty-records
  "Generate a set of custom MRSTY records using custom Concept Unique
  Identifiers (CUIs)."
  [cui]
  (let [recordlist (get-mrsty-records cui)]
    (if (empty? recordlist)
      (vector (mrsty-line-record-to-map
               (format "%s|%s|%s|%s|%s|%s|%s" cui "T999" "Z" "Unknown" "" "" "")))
      recordlist)))

(defn generate-custom-mrsty
  "Generate a custom version of MRSTY using MRSTY from MetaMorphoSys
  retaining only cuis in cuilist."
  ([src-mrstyfn cuilist dst-mrstyfn]
   (generate-custom-mrfile src-mrstyfn cuilist dst-mrstyfn))
  ([cuilist dst-mrstyfn]
   (write-custom-mrfile (flatten (mapv #(gen-mrsty-records %)
                                       cuilist))
                        rrf-mrsty/*rrf-mrsty-label-order*
                        dst-mrstyfn)))

(defn custom-mrfile-write
  "Write UMLS-like MRxxx file to writer using records with
  fieldlist as guide."
  [^Writer wtr recordlist fieldlist]
  (dorun 
   (map (fn [rrfrecord]
          (.write wtr (join "|" (mapv #(get rrfrecord %)
                                      fieldlist)))
          (.write wtr "\n"))
        (sort-by :cui recordlist))))

(defn custom-mrsty-write
  "Generate custom MRSTY file to writer."
  [mrsty-writer cuilist]
  (custom-mrfile-write
   mrsty-writer
   (flatten (mapv #(gen-mrsty-records %)
                  cuilist))
   rrf-mrsty/*rrf-mrsty-label-order*))

(defn gen-custom-mrsty-recordlist
  "Generate list custom MRSTY record strings"
  [cuilist]
  (map (fn [rrfrecord]
          (join "|" (mapv #(get rrfrecord %)
                          rrf-mrsty/*rrf-mrsty-label-order*)))
        (sort-by :cui 
                 (flatten (mapv #(gen-mrsty-records %)
                                cuilist)))))
