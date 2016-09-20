(ns synsetgen.process
  (:require [clojure.string :refer [join split trim lower-case]]
            [clojure.java.io :as io]
            [clojure.set :refer [union intersection difference]]
            [clj-time.core :refer [time-now]]
            [synsetgen.umls-indexed :as umls-indexed]
            [synsetgen.synsetgen :as synset]
            [synsetgen.umls-indexed :refer [reinit-index]]
            [synsetgen.keylistexpansion :refer [expand-termlist]])
  (:import (gov.nih.nlm.nls.nlp.nlsstrings NLSStrings)
           (java.io File)))

;; # Backend Processing Functions

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
    (synset/generate-term-cui-termset-map termlist
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
   :stt "VCW"
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
    (synset/generate-term-cui-conceptinfo-map termlist 
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

;; ## Synonym Set (synset) representation
;;
;;     user> (pprint filtered-synset)
;;     {"rlq abd pain"
;;      {"C0694551"
;;       ("Right lower quadrant pain"
;;        "abdominal pain in the right lower belly"
;;        "rlq abd pain"
;;        "lower pain quadrant right"
;;        ...
;;      )},
;;      "llq abd pain"
;;      {"C0238551"
;;       ("LLQ abdominal pain"
;;        "lower left quadrant pain"
;;        "llq pain"
;;        "abdominal pain in the left lower belly (LLQ)"
;;        "abdominal pain left lower quadrant"
;;         ...
;;      )}}
;;     nil

(defn synset-view-params-to-filtered-synset
  [params]
  (reduce (fn [newmap [key val]]
            (let [[term cui variant] (split key #"\|")]
              (assoc newmap term (assoc (newmap term) cui (conj (get (newmap term) cui) variant)))))
          {} (filterv #(= (second %) "on")
                      (dissoc params "submit"))))

(defn list-synset-cuiset
  "List cuis in synset." 
  [synset]
  (set (mapv #(-> % second first first) synset)))

(defn add-mrconso-records
  [cui mrconso-record-list termset]
  (concat mrconso-record-list 
          (map #(mrconso-record-for-term cui %)
               termset)))

(defn add-unmapped-terms-to-cui-concept-map
  "Add any unmapped-terms in synset to cui-concept-map."
  [cui-concept-map synset]
  (reduce (fn [newmap0 [parent-term cui-termlist-map]]
            (reduce (fn [newmap1 [cui termlist]]
                      (let [termset (set termlist)
                            mrconso-termset (set (map #(:str %) (newmap1 cui)))
                            missing-termset (difference termset mrconso-termset)]
                                        ; add terms in missing-termset
                        (if (empty? missing-termset)
                          newmap1
                          (assoc newmap1 cui (add-mrconso-records cui (newmap1 cui) missing-termset)))))
                    newmap0 cui-termlist-map))
          cui-concept-map synset))

(defn process-filtered-synset
  "Generate MRCONSO and MRSTY files using termlist edited by user."
  [request]
  (let [params (:params request)
        dataset (-> request :session :dataset) ; get :dataset value from :session value from request
        filtered-synset (synset-view-params-to-filtered-synset params)
        filtered-synset-cuiset (list-synset-cuiset filtered-synset)
        termlist (keys filtered-synset)
        term-conceptid-map (umls-indexed/generate-term-conceptid-map termlist)
        term-conceptid-set (union (umls-indexed/generate-term-conceptid-set termlist)
                                  filtered-synset-cuiset)
        cui-concept-map (add-unmapped-terms-to-cui-concept-map
                         (umls-indexed/generate-cui-concept-map-from-cuiset term-conceptid-set)
                         filtered-synset)
        cuiset (union (reduce (fn [newset [term concept-id-set]]
                                (union newset (set concept-id-set)))
                       #{} term-conceptid-map)
                      filtered-synset-cuiset
                      (set (keys cui-concept-map)))]
    (.mkdir (io/file (format "resources/public/output/%s" dataset))) ; create directory for session
    (synset/write-mrconso-from-cui-concept-map (format "resources/public/output/%s/mrconso.rrf" dataset)
                                                       cui-concept-map
                                                       filtered-synset)
    (synset/generate-custom-mrsty cuiset
                                          (format "resources/public/output/%s/mrsty.rrf" dataset))
    ;;(synset/generate-custom-mrsat (str "input/" *umls-version* "/MRSAT.RRF")
    ;; cuiset
    ;;"resources/public/output/mrsat.rrf")
    ))

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
