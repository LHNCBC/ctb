(ns ctb.process
  (:require [clojure.string :refer [join split trim lower-case]]
            [clojure.java.io :as io]
            [clojure.set :refer [union intersection difference]]
            [clojure.tools.logging :as log]
            [clj-time.core :refer [time-now]]
            [digest]
            [ctb.umls-indexed :as umls-indexed]
            [ctb.synsetgen :as synset]
            [ctb.umls-indexed :refer [init-index]]
            [ctb.keylistexpansion :refer [termlist-info
                                          init-lvg
                                          termlist-info-with-lvg]])
  (:import (java.util Properties)
           (javax.servlet ServletContext)
           (java.io File))
  (:gen-class))

;; # Backend Processing Functions

;;
;; Servlet Context Variables
;;
(def ^:dynamic *context-root-path* "")
(def ^:dynamic *context-config-path* "config")
(def ^:dynamic *context-data-path* "data")

;; default data set paths and properties

(def ^:dynamic *umls-version* "2016AA")
(def ^:dynamic *ivfdirname* "data/ivf")
(def ^:dynamic *default-ivf-release-dirname* "data/ivf/2016AA")
(def ^:dynamic  ^Properties *properties* (new Properties))
(def ^:dynamic *lvg-initialized* false)

(defn set-context-root-path
  [root-path]
  (def ^:dynamic *context-root-path* root-path))

(defn set-context-config-path
  [config-path]
  (def ^:dynamic *context-config-path* config-path))

(defn set-context-data-path
  [data-path]
  (def ^:dynamic *context-data-path* data-path))

(defn resolve-path
  "If path is with-in server context then use it; otherwise, don't
  modify path."
  [path]
  (cond
    (.exists (io/file (str *context-root-path* path)))       (str *context-root-path* path)
    (.exists (io/file (str *context-data-path* "/" path)))   (str *context-data-path* "/" path)
    (.exists (io/file (str *context-config-path* "/" path))) (str *context-config-path* "/" path)
    :else path))

(defn init
  "Initialize any needed resources"
  []
  ;; Load CBT properties from config/ctb.properties (settable by system
  ;; property "ctb.property.file")  (should this be in ctb.webapp/init?)
  (let [config-file-path (resolve-path
                          (System/getProperty "ctb.property.file"
                                              "ctb.properties"))]
    (log/info (format "config-file-path: %s" config-file-path))
    (if (.exists (io/file config-file-path))
      (.load *properties* (io/reader config-file-path))
      (log/error (format "ctb.process/init: config file %s does not exist." config-file-path)))
    (let [ivf-dataroot (resolve-path (.getProperty *properties* "ctb.ivf.dataroot"))
          lvg-path (.getProperty *properties* "ctb.lvg.directory")
          lvg-directory (resolve-path lvg-path)]
      (log/info (format "ctb.ivf.dataroot: %s" ivf-dataroot))
      (log/info (format "ctb.lvg.directory: %s" lvg-directory))
      ;; inverted file initialization
      (if (.exists (io/file ivf-dataroot))
        (init-index ivf-dataroot "tables" "ifindices")
        (log/error (format "ctb.process/init: data root file %s does not exist." ivf-dataroot)))
      ;; lvg initialization
      (if lvg-path
        (do
          (init-lvg lvg-directory)
          (def ^:dynamic *lvg-initialized* true))
        (do 
          (log/error (format "ctb.process/init: lvg directory %s does not exist." lvg-directory))
          (def ^:dynamic *lvg-initialized* false)))
      )))

(defn print-request
  [request]
  (dorun (map (fn [[k v]]
                (println (format "%s -> %s" k v)))
              request)))

(defn mirror-termlist
  "Function for testing termlist web form."
  [termlist]
  (filter #(> (count (trim %)) 0)
          (split termlist #"[\r\n]")))

(defn process-termlist-v1
  "Generate term -> cui -> synonym termset map."
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
  "Convert list of terms separated by newlines into vector of terms,
  removing any empty terms."
  [termlist-string]
  (filterv #(> (count (trim %)) 0)
           (split termlist-string #"[\r\n]")))

(defn mrconso-record-for-term [cui term]
  "Create custom mrconso record for cui and term."
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
  "Combine contents of two cui -> concept info maps."
  [cui-concept-map0 cui-concept-map1]
  (reduce (fn [newmap [cui recordlist]]
            (assoc newmap cui (concat (newmap cui) recordlist)))
            cui-concept-map0 cui-concept-map1))

(defn generated-unmapped-term-cui-concept-map
  [unmapped-term-expanded-conceptid-map]
  (reduce (fn [newmap [term terminfomap]]
            (merge newmap (into {} (mapv (fn [cui]
                                           (vector cui (vector (mrconso-record-for-term cui term))))
                                         (:cuilist terminfomap)))))
          {} unmapped-term-expanded-conceptid-map))

(defn process-termlist
  "Using termlist supplied by input terms form, generate term-> cui ->
  conceptinfo map suitable for converting into expanded termlist
  collapsible tree."
  [dataset newtermlist]
  (let [termlist (if (string? newtermlist)
                   (termlist-string-to-vector newtermlist)
                   newtermlist)
        term-conceptid-map (umls-indexed/generate-term-conceptid-map termlist)
        term-conceptid-set (reduce #(union %1 %2) (vals term-conceptid-map))
        unmapped-terms (mapv #(first %) (filterv #(empty? (second %)) term-conceptid-map))
        unmapped-term-expanded-info-map (if *lvg-initialized*
                                          (termlist-info-with-lvg unmapped-terms)
                                          (termlist-info unmapped-terms))
        unmapped-term-expanded-conceptid-map (into {} (mapv #(vector (first %) (:cuilist (second %)))
                                                            unmapped-term-expanded-info-map))
        unmapped-term-expanded-cuiset (reduce (fn [newset item]
                                                (union newset (:cuilist (second item))))
                                              #{} unmapped-term-expanded-info-map)
        cui-concept-map (umls-indexed/generate-cui-concept-map-from-cuiset (union term-conceptid-set
                                                                                  unmapped-term-expanded-cuiset))
        unmapped-term-cui-concept-map (generated-unmapped-term-cui-concept-map
                                       unmapped-term-expanded-conceptid-map)
        merged-cui-concept-map (merge-cui-concept-maps cui-concept-map unmapped-term-cui-concept-map)
        
        ]
    (synset/generate-term-cui-conceptinfo-map termlist 
                                              (merge term-conceptid-map
                                                     unmapped-term-expanded-conceptid-map)
                                              merged-cui-concept-map
                                              unmapped-term-expanded-info-map)))

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
  ([req]
   (let [^ServletContext servlet-context (:servlet-context req)
         tmpfolder (if servlet-context
                     (.getAttribute servlet-context ServletContext/TEMPDIR)
                     "resources/public/output")]
     (write-filtered-termlist (str tmpfolder "/filtered-termlist.edn") req)))
  ([filename req]
   (spit filename (pr-str (dissoc (:params req) "submit")))))       ; remove submit before writing

(defn list-term-synonyms
  [params]
  (sort 
   (into []
         (set
          (mapv (fn [[key val]]
                  (let [[term cui synonym] (split key #"\|")]
                    (lower-case synonym)))
                (filterv #(= (second %) "on")
                         (dissoc params "submit")))))))

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
  "Extract terms for synset from request parameters excluding submit
  parameter."
  [params]
  (reduce (fn [newmap [key val]]
            (let [[term cui synonym] (split key #"\|")]
              (assoc newmap term (assoc (newmap term) cui (conj (get (newmap term) cui) synonym)))))
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
  ([request]
   (let [params (:params request)
         ^ServletContext servlet-context (:servlet-context request)
         user (-> request :cookies (get "termtool-user") :value)
         dataset (-> request :session :dataset) ; Get :dataset and :user values
                                                ; from :session part of request.
         filtered-synset (synset-view-params-to-filtered-synset params)
         termlist (keys filtered-synset)
         synonyms-checksum (digest/sha-1 (join "|" (list-term-synonyms params)))
         tmpfolder (if servlet-context
                     (.getAttribute servlet-context ServletContext/TEMPDIR)
                     "resources/public/output")
         workdir (format "%s/%s/%s" tmpfolder user dataset)]
     ;; (print-request request)
     
     ;; if result already exists with the same filtered termlist
     ;; checksum then don't process it again.
     (if (.exists (io/file workdir))
       (if (.exists (io/file (str workdir "/synonyms.checksum")))
         (do
           (let [saved-synonyms-checksum (slurp (str workdir "/synonyms.checksum"))]
             (when (not= saved-synonyms-checksum synonyms-checksum)
               (process-filtered-synset user dataset workdir termlist synonyms-checksum
                                        filtered-synset)
               )))
         (process-filtered-synset user dataset workdir termlist synonyms-checksum
                                  filtered-synset)
         )
         (do
           (.mkdirs (io/file workdir))
           (write-filtered-termlist (str workdir "/filtered-termlist.edn") request)
           (spit (str workdir "/params.txt") (join "\n" (keys params)))
           (process-filtered-synset user dataset workdir termlist synonyms-checksum
                                    filtered-synset))
     )))
  ([user dataset workdir
    termlist synonyms-checksum filtered-synset]
   (let [filtered-synset-cuiset (list-synset-cuiset filtered-synset)
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
                                        ; create directory for session
     (spit (str workdir "/termlist.txt") (join "\n" termlist))
     (spit (str workdir "/filtered-synset.edn") (pr-str filtered-synset))
     (synset/write-mrconso-from-cui-concept-map (str workdir "/mrconso.rrf")
                                                cui-concept-map
                                                filtered-synset)
     (synset/generate-custom-mrsty cuiset
                                   (str workdir "/mrsty.rrf"))
     (spit (format (str workdir "/synonyms.checksum")) synonyms-checksum)
     ;;(synset/generate-custom-mrsat (str "input/" *umls-version* "/MRSAT.RRF")
     ;;   cuiset 
     ;;  (str workdir "/mrsat.rrf"))
     )))



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
  (init-index ivfpath "tables" "ifindices"))

(defn contains-ifindices
  "Does directory contain ifindices?"
  [^File dirfile]
  (not
   (empty?
    (filter (fn [^File subdirfile]
              (when (= (:name (bean subdirfile)) "ifindices")
                (not (empty? (.listFiles subdirfile)))))
            (.listFiles dirfile)))))

(defn list-data-set-names
  "List names of currently installed datasets"
  []
  (let [ivfdir (File. ^String *ivfdirname*)]
    (mapv #(:name (bean %))
          (filterv contains-ifindices
                   (filterv #(:directory (bean %))
                            (into [] (.listFiles ivfdir)))))))
