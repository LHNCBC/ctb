(ns synsetgen.core
  (:require [clojure.java.io :as io]
            [clojure.string :refer [join split]]
            [clojure.set :refer [union]]
            [skr.rrf-mrconso-utils :as rrf-mrconso]
            [skr.rrf-mrsty-utils :as rrf-mrsty]
            [skr.mwi-utilities :as mwi]
            [synsetgen.umls-indexed :refer [generate-term-conceptid-map
                                            get-preferred-name]]))

;; # Synonym Set Generation (synsetgen)

(defonce ^:dynamic *memoized-normalize-ast-string* (memoize mwi/normalize-ast-string))
(defonce ^:dynamic *default-sab* "custom")

(defn term-cui-termset-map-to-cui-termset-map
  "Convert term -> cui -> termset map to cui -> termset map"
  [term-cui-termset-map]
  (reduce (fn [newmap [term cui-termset-map]]
            (reduce (fn [submap [cui termset]]
                      (assoc submap cui (union (submap cui) (cui-termset-map cui))))
                    newmap cui-termset-map))
          {} term-cui-termset-map))
  
(defn write-mrconso-from-cui-concept-map
  "Write MRCONSO records in cui-concept-map to file"
  ([filename cui-concept-map]
   (with-open [wtr (io/writer filename)]
     (dorun
      (map (fn [cui]
             (dorun
              (map (fn [record]
                     (.write wtr (format "%s\n"(rrf-mrconso/mrconso-map-to-line-record record))))
                   (cui-concept-map cui))))
           (sort (keys cui-concept-map))))))
  ([filename cui-concept-map filtered-synset]
   (let [cui-termset (term-cui-termset-map-to-cui-termset-map filtered-synset)]
     (with-open [wtr (io/writer filename)]
       (dorun
        (map (fn [cui]
               (dorun
                (map (fn [record]
                       (when (contains? (set (cui-termset cui)) (:str record))
                         (.write wtr (format "%s\n"(rrf-mrconso/mrconso-map-to-line-record record)))))
                     (cui-concept-map cui))))
             (sort (keys cui-concept-map))))))))
  
(defn generate-term-cui-termset-map
  "Generate map of term -> cui -> termset from termlist, term-conceptid-map,
  and cui-concept-map."
  [termlist term-conceptid-map cui-concept-map]
  (reduce (fn [term-map term]
            (let [nmterm (*memoized-normalize-ast-string* term)
                  cuiset (term-conceptid-map nmterm)]
              (assoc term-map term
                     (reduce (fn [synset-map cui]
                               (assoc synset-map cui
                                      (set
                                       (mapv #(:str %)
                                             (cui-concept-map cui)))))
                             {} cuiset) )))
          {} termlist))

(defn generate-term-cui-conceptinfo-map
  "Generate map of term -> synsets from termlist, term-conceptid-map,
  and cui-concept-map."
  [termlist term-conceptid-map cui-concept-map]
  (reduce (fn [term-map term]
            (let [nmterm (*memoized-normalize-ast-string* term)
                  cuiset (term-conceptid-map nmterm)]
              (if (> (count cuiset) 0)
                (assoc term-map term
                       (reduce (fn [synset-map cui]
                                 (assoc synset-map
                                        cui {:preferred-name (get-preferred-name cui)
                                             :termset (set
                                                       (mapv #(:str %)
                                                             (cui-concept-map cui)))}
                                        ))
                               {} cuiset) )
                (assoc term-map term {"unknown concept" {:preferred-name term
                                                         :termset #{ term }}})) ))
          {} termlist))

;;
;; The function _inc-cui-index_ should be replaced by functional code that generates
;; a set of synthetic mrconso records using collected strings that
;; don't occur in knowledge source.
;;

(def ^:dynamic *cui-index* 1)

(defn inc-cui-index
  "increment global cui index"
  []
  (let [cui-index *cui-index*]
    (def ^:dynamic *cui-index* (inc *cui-index*))
    cui-index))

(defn generate-synthetic-mrconso-record
  "make a synthetic mrconso record"
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
    "" ;; (format "L%8d" (inc-lui-index))
    "" ;; (format "S%8d" (inc-str-index))
    "" ;; (format "A%8d" (inc-aui-index))
    *default-sab*
    ""
    ""
    term))
  ([term]
   (generate-synthetic-mrconso-record
    (format "Z%07d" (inc-cui-index))
    "" ;; (format "L%8d" (inc-lui-index))
    "" ;; (format "S%8d" (inc-str-index))
    "" ;; (format "A%8d" (inc-aui-index))
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
                     (if (contains? (set (map *memoized-normalize-ast-string* termset))
                                    (*memoized-normalize-ast-string* term))
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
                                (if (contains? (set (map *memoized-normalize-ast-string* termset))
                                               (*memoized-normalize-ast-string* term))
                                  (assoc cmap cui (cui-concept-map cui))
                                  (assoc cmap cui (conj (cui-concept-map cui)
                                                        (generate-synthetic-mrconso-record term)))))
                              {} cui-termset-map))))
           {} term-synset-map))

(defn serialize-augmented-mrconso-records-map
  "Serialize augmented MRCONSO records to file."
  [filename augmented-mrconso-records-map]
  (with-open [wtr (io/writer filename)]
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
  "list any term not mapped to any concepts."
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
      (with-open [wtr (io/writer dst-mrfilefn)]
        (dorun 
         (map (fn [line]
                (let [fields (split line #"\|")]
                  (if (contains? cuiset (first fields))
                    (.write wtr (format "%s\n" line)))))
              (line-seq rdr)))))))

(defn generate-custom-mrrank
  [src-mrrankfn custom-sabset]
  (with-open [rdr (io/reader src-mrrankfn)]
    ;; to be implemented
 ))

(defn generate-custom-mrsab
  [src-mrsabfn cuilist dst-mrsabfn] 
   (generate-custom-mrfile src-mrsabfn cuilist dst-mrsabfn))
  


(defn generate-custom-mrsat
  ([src-mrsatfn cuilist dst-mrsatfn] 
   (generate-custom-mrfile src-mrsatfn cuilist dst-mrsatfn))
  ([cuilist dst-mrsatfn]
   (mapv #()
         cuilist)))

(defn generate-custom-mrsty
  ([src-mrstyfn cuilist dst-mrstyfn]
   (generate-custom-mrfile src-mrstyfn cuilist dst-mrstyfn))
  ([cuilist dst-mrstyfn]
   (mapv #()
         cuilist)))

