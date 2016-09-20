(ns synsetgen.umls-raw
  (:require [clojure.java.io :as io]
            [clojure.string :refer [join split]]
            [skr.rrf-mrconso-utils :as rrf-mrconso]
            [skr.rrf-mrsty-utils :as rrf-mrsty]
            [skr.mwi-utilities :as mwi]))

;; # Functions to process raw UMLS files

(defonce ^:dynamic *memoized-normalize-ast-string* (memoize mwi/normalize-ast-string))

(defn load-mrconso
  [mrconsofn]
  (filter #(= (:lat %) "ENG")
          (mapv rrf-mrconso/mrconso-line-record-to-map
                (rrf-mrconso/load-unsuppressed-mrconso mrconsofn))))

;; term -> concept list
;; concept: synset and other info

(defn generate-cui-concept-map
  "Generate cui -> concept-records map from mrconso list."
  [mrconso]
  (reduce (fn [newmap record]
            (assoc newmap (:cui record) (conj (newmap (:cui record)) record)))
          {} mrconso))

(defn generate-nmstr-concept-list-map
  "Generate a normalized string -> set of cui|concept records for the purpose of containing synsets."
  ([mrconso cui-concept-map]
   (reduce (fn [newmap record]
             (let [nmstr (*memoized-normalize-ast-string* (:str record))]
               (if (contains? newmap nmstr)
                 (assoc newmap nmstr (conj (newmap nmstr) (cui-concept-map (:cui record))))
                 (assoc newmap nmstr (set (cui-concept-map (:cui record)))))))
           {} mrconso))
  ([mrconso]
   (generate-nmstr-concept-list-map mrconso (generate-cui-concept-map mrconso))))

(defn generate-term-conceptid-map
  "Generate map of term -> conceptids"
  [mrconso termlist]
  (let [termset (set (map *memoized-normalize-ast-string* termlist))]
    (reduce (fn [newmap record]
              (let [nmstr (*memoized-normalize-ast-string* (:str record))]
                (if (contains? termset nmstr)
                  (if (contains? newmap nmstr)
                    (assoc newmap nmstr (conj (newmap nmstr) (:cui record)))
                    (assoc newmap nmstr (set (vector (:cui record)))))
                  newmap)))
            {} mrconso)))

(defn generate-term-conceptid-set
  "Generate set of conceptids corresponding to termlist"
  [mrconso termlist]
  (let [termset (set (map *memoized-normalize-ast-string* termlist))]
    (reduce (fn [newset record]
              (let [nmstr (*memoized-normalize-ast-string* (:str record))]
                (if (contains? termset nmstr)
                  (conj newset (:cui record))
                  newset)))
            #{} mrconso)))

(defn generate-cui-concept-map-from-cuiset
  "Generate cui -> concept-records map."
  [mrconso cuiset]
  (reduce (fn [newmap record]
            (if (contains? cuiset (:cui record))
              (assoc newmap (:cui record) (conj (newmap (:cui record)) record))
              newmap))            
          {} mrconso))
