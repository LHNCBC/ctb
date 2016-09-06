(ns synsetgen.process
  (:require [clojure.string :refer [join split trim lower-case]]
            [clojure.java.io :as io]
            [clojure.set :refer [union]]
            [clj-time.core :refer [time-now]]
            [synsetgen.umls-indexed :as umls-indexed]
            [synsetgen.core]
            [synsetgen.umls-indexed :refer [reinit-index]]
            [synsetgen.keylistexpansion :refer [expand-termlist]])
  (:import (gov.nih.nlm.nls.nlp.nlsstrings NLSStrings)
           (java.io File)))

;; # Processing Backend Functions

(def ^:dynamic *umls-version* "2016AA")
(def ^:dynamic *ivfdirname* "data/ivf")
(def ^:dynamic *default-ivf-release-dirname* "data/ivf/2016AA")
(defn init
  []
  (reinit-index *default-ivf-release-dirname* "tables" "ifindices"))

(defn mirror-termlist
  [termlist]
  (filter #(> (count (trim %)) 0)
          (split termlist #"[\r\n]")))

(defn process-termlist-v1
  [newtermlist]
  (let [termlist (filter #(> (count (trim %)) 0)
                       (split newtermlist #"[\r\n]"))
        term-conceptid-map (umls-indexed/generate-term-conceptid-map termlist)
        term-conceptid-set (umls-indexed/generate-term-conceptid-set termlist)
        cui-concept-map (umls-indexed/generate-cui-concept-map-from-cuiset term-conceptid-set)]
    (synsetgen.core/generate-term-cui-termset-map termlist
                                                  term-conceptid-map
                                                  cui-concept-map)))

(defn termlist-string-to-vector
  [termlist-string]
  (filterv #(> (count (trim %)) 0)
           (split termlist-string #"[\r\n]")))

(defn mrconso-record-for-term [cui term]
  {:cui cui
   :lat "ENG"
   :ts "S"
   :lui "LXXXXXXX"
   :stt ""
   :sui "SXXXXXXX"
   :ispref "N"
   :aui "AXXXXXXX"
   :saui ""
   :scui ""
   :sdui ""
   :sab "custom"
   :tty "SY"
   :code ""
   :str term
   :srl ""
   :suppress "N"
   :cvf ""
})

(defn merge-cui-concept-maps
  [cui-concept-map0 cui-concept-map1]
  (reduce (fn [newmap [cui recordlist]]
            (assoc newmap cui (concat (newmap cui) recordlist)))
            cui-concept-map0 cui-concept-map1))

(defn generated-unmapped-term-cui-concept-map
  [unmapped-term-expanded-conceptid-map]
  (reduce (fn [newmap [term cuiset]]
            (merge newmap (into {} (mapv (fn [cui]
                                           (vector cui (vector (mrconso-record-for-term cui term))))
                                         cuiset))))
          {} unmapped-term-expanded-conceptid-map))

(defn process-termlist
  "Using termlist supplied by input terms form, generate term-> cui ->
  conceptinfo map suitable for converting into expanded termlist
  collapsible tree."
  [newtermlist]
  (let [termlist (if (string? newtermlist)
                   (termlist-string-to-vector newtermlist)
                   newtermlist)
        term-conceptid-map (umls-indexed/generate-term-conceptid-map termlist)
        term-conceptid-set (umls-indexed/generate-term-conceptid-set termlist)
        unmapped-terms (mapv #(first %) (filterv #(empty? (second %)) term-conceptid-map))
        unmapped-term-expanded-conceptid-map (expand-termlist unmapped-terms)
        unmapped-term-expanded-cuiset (reduce (fn [newset item]
                                                (union newset (second item)))
                                                #{} unmapped-term-expanded-conceptid-map)
        cui-concept-map (umls-indexed/generate-cui-concept-map-from-cuiset (union term-conceptid-set
                                                                                  unmapped-term-expanded-cuiset))
        unmapped-term-cui-concept-map (generated-unmapped-term-cui-concept-map
                                       unmapped-term-expanded-conceptid-map)
        merged-cui-concept-map (merge-cui-concept-maps cui-concept-map unmapped-term-cui-concept-map)
        ]
    (synsetgen.core/generate-term-cui-conceptinfo-map termlist 
                                                      (merge term-conceptid-map
                                                             unmapped-term-expanded-conceptid-map)
                                                      merged-cui-concept-map)))

(defn process-termlist-and-termlistfile
  "Combine termlists from input box and file if either exists"
  [termlist termlistfile]
  (let [combined-termlist (concat
                           (if (string? termlist)
                             (termlist-string-to-vector termlist)
                             termlist)
                           (if (string? termlistfile)
                             (termlist-string-to-vector termlistfile)
                             ""))]
    (process-termlist combined-termlist)))


(defn write-filtered-termlist
  [req]
  (with-open [wtr (io/writer "resources/public/output/filtered-termlist.edn")]
    (.write wtr (pr-str (dissoc (:params req) "submit")))))       ; remove submit before writing

(defn synset-view-params-to-filtered-synset
  [params]
  (reduce (fn [newmap [key val]]
            (let [[term cui variant] (split key #"\|")]
              (assoc newmap term (assoc (newmap term) cui (conj (get (newmap term) cui) variant)))))
          {} (filterv #(= (second %) "on")
                      (dissoc params "submit"))))

(defn process-filtered-synset
  ""
  [req]
  (let [params (:params req)
        filtered-synset (synset-view-params-to-filtered-synset params)
        termlist (keys filtered-synset)
        term-conceptid-map (umls-indexed/generate-term-conceptid-map termlist)
        term-conceptid-set (umls-indexed/generate-term-conceptid-set termlist)
        cui-concept-map (umls-indexed/generate-cui-concept-map-from-cuiset term-conceptid-set)
        cuiset (reduce (fn [newset [term concept-id-set]]
                         (union newset (set concept-id-set)))
                       #{} term-conceptid-map)]
    (synsetgen.core/write-mrconso-from-cui-concept-map "resources/public/output/mrconso.rrf"
                                                       cui-concept-map
                                                       filtered-synset)
    (synsetgen.core/generate-custom-mrsat (str "input/" *umls-version* "/MRSAT.RRF")
                                          cuiset
                                          "resources/public/output/mrsat.rrf")
    (synsetgen.core/generate-custom-mrsty (str "input/" *umls-version* "/MRSTY.RRF")
                                          cuiset
                                          "resources/public/output/mrsty.rrf")
    ))

(defn syntactically-simple?
  "Is string syntactically-simple and contains no NOS or NEC or
  multiple meaning designators.

  Not the same as the function isSyntacticallySimple which determines
  the number of Minimal Syntactic Units (noMSUs) present in a string
  where the number of Minimal Syntactic Units is below some
  pre-determined threshold.

"
  [term]
  (and
   (= (NLSStrings/eliminateNosString term) (lower-case term))
   (= (NLSStrings/eliminateMultipleMeaningDesignatorString term) term)
   ;; (not (NLSStrings/containsPrepOrConj term))
   (not (NLSStrings/abgn_form term))
  ))

(def ifconfig-base
  "NUM_TABLES: 3
zzsty|mrsty|2|0|cui|semtype|TXT|TXT
MRCONSO.RRF.cui|mrconso|18|0|cui|lat|termstatus|lui|termtype|SUI|ISPREF|AUI|SAUI|SCUI|SDUI|SAB|TTY|CODE|string|SRL|SUPPRESS|CVF|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT
zz.name.lbld|zz|4|1|label|casename|termstatus|ambiguity|TXT|TXT|TXT|TXT
MRCONSO.RRF|mrconsostr|18|14|cui|lat|termstatus|lui|termtype|SUI|ISPREF|AUI|SAUI|SCUI|SDUI|SAB|TTY|CODE|string|SRL|SUPPRESS|CVF|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT|TXT
")

(defn index-umls-data-set
  [ivfpath]
  (with-open [wtr (io/writer (format "%s/tables/ifconfig" ivfpath))]
    (spit wtr ifconfig-base))
  (reinit-index ivfpath "tables" "ifindices"))

(defn contains-ifindices
  "Does directory contain ifindices?"
  [dirfile]
  (not
   (empty?
    (filter (fn [subdirfile]
              (when (= (:name (bean subdirfile)) "ifindices")
                (not (empty? (.listFiles subdirfile)))))
            (.listFiles dirfile)))))

(defn list-data-set-names
  "List names of currently installed datasets"
  []
  (let [ivfdir (File. *ivfdirname*)]
    (mapv #(:name (bean %))
          (filterv contains-ifindices
                   (filterv #(:directory (bean %))
                            (into [] (.listFiles ivfdir)))))))
