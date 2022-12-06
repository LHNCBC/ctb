(ns ctb.process
  (:require [clojure.string :refer [join split trim lower-case index-of
                                    replace-first]]
            [clojure.java.io :as io]
            [clojure.set :refer [union intersection difference]]
            [clojure.tools.logging :as log]
            [ring.util.io :refer [piped-input-stream]]
            [digest]
            [hiccup.util :refer [escape-html]]
            [ctb.umls-indexed :as umls-indexed]
            [ctb.synsetgen :as synset]
            [ctb.umls-indexed :refer [init-index]]
            [ctb.backend :refer [init
                                 init-using-servlet-context
                                 init-standalone]]
            [ctb.keylistexpansion :refer [termlist-info
                                          termlist-info-with-lvg]]
            [ctb.ring-utils :refer [TEMPDIR get-context-attribute]])
  (:import (java.lang Boolean)
           (java.util Properties)
           (javax.servlet ServletContext)
           (java.io File Reader Writer)
           (org.owasp.encoder Encode))
  (:gen-class)
  )

;; Place a reference to application context here.  Currently,
;; application context is retrieved from servlet context when using Apache
;; Tomcat.
(def appcontext (atom {}))

(defn get-appcontext-original
  "Attempt to get application context from servlet context,
  if not available, check for context in atom, if not available, the
  create new application context, place in new context atom and then
  return it."
  [request]
  (let [servlet-ctbappcontext (get-context-attribute request "ctbappcontext")]
    (cond servlet-ctbappcontext servlet-ctbappcontext
          @appcontext @appcontext 
          :else (let [new-app-context (init)]
                  (when (nil? new-app-context)
                    (do (log/error "error creating application context.")))))))

(defn get-appcontext
  "just initialize a new context (this is wasteful.)"
  [request]
  (log/info "request keys: " (keys request))
  (if (contains? request :servlet-context)
    (do (log/warn "servlet-context: " (:servlet-context request))
        (init ^ServletContext (:servlet-context request)))
    (do (log/warn "calling init-standalone...")
        (init-standalone))))

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
  [appcontext dataset newtermlist]
  (let [termlist (if (string? newtermlist)
                   (termlist-string-to-vector newtermlist)
                   newtermlist)
        term-conceptid-map (umls-indexed/generate-term-conceptid-map (:ivf-indexes appcontext) termlist)
        term-conceptid-set (reduce #(union %1 %2) (vals term-conceptid-map))
        unmapped-terms (mapv #(first %) (filterv #(empty? (second %)) term-conceptid-map))
        unmapped-term-expanded-info-map (if (:lvg-initialized appcontext)
                                          (termlist-info-with-lvg (:lvg-api appcontext)
                                                                  (:ivf-indexes appcontext) unmapped-terms)
                                          (termlist-info (:lvg-api appcontext)
                                                         (:ivf-indexes appcontext) unmapped-terms))
        unmapped-term-expanded-conceptid-map (into {} (mapv #(vector (first %) (:cuilist (second %)))
                                                            unmapped-term-expanded-info-map))
        unmapped-term-expanded-cuiset (reduce (fn [newset item]
                                                (union newset (:cuilist (second item))))
                                              #{} unmapped-term-expanded-info-map)
        cui-concept-map (umls-indexed/generate-cui-concept-map-from-cuiset (:ivf-indexes appcontext)
                                                                           (union term-conceptid-set
                                                                                  unmapped-term-expanded-cuiset))
        unmapped-term-cui-concept-map (generated-unmapped-term-cui-concept-map
                                       unmapped-term-expanded-conceptid-map)
        merged-cui-concept-map (merge-cui-concept-maps cui-concept-map
                                                       unmapped-term-cui-concept-map)
        
        ]
    (synset/generate-term-cui-conceptinfo-map (:ivf-indexes appcontext)
                                              termlist 
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

;; servletContext.getAttribute("javax.servlet.context.tempdir")
(defn get-servlet-context-tempdir
  "Get temporary storage directory from servlet context."
  [^ServletContext servlet-context]
  (let [tempdir (if servlet-context
                  (do (.getPath (io/file (.getAttribute servlet-context TEMPDIR)))
                      (log/debug "(.getAttribute servlet-context TEMPDIR): "
                                 (.getAttribute servlet-context TEMPDIR))
                      (log/debug "(.getAttribute servlet-context \"javax.servlet.context.tempdir\"): "
                                 (.getPath (io/file (.getAttribute servlet-context "javax.servlet.context.tempdir")))))
                  "resources/public/output")]
    (if (nil? tempdir)
      "/tmp"
      tempdir)))

(defn sanitize-src-cui-derived
  "sanitize string of form:
       src|cui|derived "
  [src-cui-derived]
  (let [[srcstring cui derivedstring] (split src-cui-derived #"\|")]
    (join "|" (vector (Encode/forCDATA srcstring)
                      (Encode/forCDATA cui)
                      (Encode/forCDATA derivedstring)))))

(defn sanitize-params
  [params-map]
  (into {} 
        (mapv (fn [[k v]]
               (let [k-parameter (sanitize-src-cui-derived k)]
                 (vector k-parameter
                         (Encode/forCDATA (str v)))))
              ;; remove submit before sanitizing and only include "on" terms
              (filterv #(= (second %) "on")
                       (dissoc params-map "submit")))))

(defn write-filtered-termlist
  ([req]
   (let [^ServletContext servlet-context (:servlet-context req)
         tmpfolder (get-servlet-context-tempdir servlet-context)]
     (write-filtered-termlist (str tmpfolder "/filtered-termlist.edn") req)))
  ([filename req]
   (let [params-map (sanitize-params (:params req))]
     (spit filename (pr-str params-map)))))

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
   (let [params (sanitize-params (:params request))
         ^ServletContext servlet-context (:servlet-context request)
         appcontext (get-appcontext request)
         ;; Get :dataset and :user values from :session part of request.
         user (-> request :cookies (get "termtool-user") :value)
         dataset (-> request :session :dataset) 
         filtered-synset (synset-view-params-to-filtered-synset params)
         termlist (keys filtered-synset)
         synonyms-checksum (digest/sha-1 (join "|" (list-term-synonyms params)))
         tmpfolder (get-servlet-context-tempdir servlet-context)
         workdir (format "%s/%s/%s" tmpfolder user dataset)]
     ;; (print-request request)
     
     ;; if result already exists with the same filtered termlist
     ;; checksum then don't process it again.
     (if (.exists (io/file workdir))
       (if (.exists (io/file (str workdir "/synonyms.checksum")))
         (do
           (let [saved-synonyms-checksum (slurp (str workdir "/synonyms.checksum"))]
             (when (not= saved-synonyms-checksum synonyms-checksum)
               (process-filtered-synset appcontext user dataset workdir termlist synonyms-checksum
                                        filtered-synset)
               )))
         (process-filtered-synset appcontext user dataset workdir termlist synonyms-checksum
                                  filtered-synset)
         )
       (do
         (.mkdirs (io/file workdir))
         (write-filtered-termlist (str workdir "/filtered-termlist.edn") request)
         (spit (str workdir "/params.txt") (join "\n" (keys params)))
         (process-filtered-synset appcontext user dataset workdir termlist synonyms-checksum
                                  filtered-synset))
       )))
  ([appcontext user dataset workdir
    termlist synonyms-checksum filtered-synset]
   (let [filtered-synset-cuiset (list-synset-cuiset filtered-synset)
         indexes (:ivf-indexes appcontext)
         term-conceptid-map (umls-indexed/generate-term-conceptid-map indexes termlist)
         term-conceptid-set (union (umls-indexed/generate-term-conceptid-set indexes termlist)
                                   filtered-synset-cuiset)
         cui-concept-map (add-unmapped-terms-to-cui-concept-map
                          (umls-indexed/generate-cui-concept-map-from-cuiset indexes term-conceptid-set)
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
     (synset/generate-custom-mrsty indexes cuiset
                                   (str workdir "/mrsty.rrf"))
     (spit (format (str workdir "/synonyms.checksum")) synonyms-checksum)
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
  (let [ivfdir (File. ^String (:ivf-dataroot @appcontext))]
    (mapv #(:name (bean %))
          (filterv contains-ifindices
                   (filterv #(:directory (bean %))
                            (into [] (.listFiles ivfdir)))))))

(defn is-valid-path?
  "Is url path valid? return true if valid, false otherwise."
  [request path]
  (let [tempdir (get-servlet-context-tempdir (:servlet-context request))
        cookies (:cookies request)
        session (:session request)
        user (cond
               (contains? session :user) (:user session)
               (contains? cookies "termtool-user") (-> cookies
                                                       (get "termtool-user")
                                                       :value)
                      :else "NoUserName")
        derived-path (str tempdir "/" user
                          (replace-first path "/dataset" "/"))
        file-exists (.exists (io/file derived-path))
        contains-no-ellipsis (nil? (index-of derived-path ".."))]
    ;; (.println System/out (str "path: " path))
    ;; (.println System/out (str "tempdir: " tempdir))
    ;; (.println System/out (str "user: " user))
    ;; (.println System/out (str "derived-path: " derived-path))
    ;; (.println System/out (str "file-exists: " file-exists))
    (and file-exists contains-no-ellipsis)))

(defn render-path
  "Is url path valid? return string \"invalid-path\" if invalid, the
  original path if valid."
  [request path]
  (if (is-valid-path? request path)
    path
    "invalid-path"))

(defonce hex-pattern (re-pattern "^[\\dA-Fa-f]+$"))

(defn is-hex-number?
  "Is string a hex number?"
  [text-string]
  (re-find hex-pattern text-string))

(def hex-map
  #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9
    \A \B \C \D \E \F
    \a \b \c \d \e \f})

(defn encode-dataset-key
  "make string a valid hex number"
  [text-string]
  (filter #(contains? hex-map %)
          text-string))

(defn termlist-to-cui-concept-map
  "Given a termlist, generate a synset from the umls and generate a
  cui-concept-map to be later converted to mrconso."
  [newtermlist]
  (let [termlist (if (string? newtermlist)
                   (termlist-string-to-vector newtermlist)
                   newtermlist)
        term-synset (process-termlist "dataset" termlist)
        synset-cuiset (list-synset-cuiset term-synset)
        term-conceptid-set (union (umls-indexed/generate-term-conceptid-set
                                   termlist)
                                  synset-cuiset)]
    (add-unmapped-terms-to-cui-concept-map
     (umls-indexed/generate-cui-concept-map-from-cuiset term-conceptid-set)
     term-synset)))

(defn termlist-to-cuiset
  "Given a termlist, generate a synset from the umls and generate a
  cuiset to be later converted to mrsty."
  [newtermlist]
  (let [termlist (if (string? newtermlist)
                   (termlist-string-to-vector newtermlist)
                   newtermlist)
        term-synset (process-termlist "dataset" termlist)]
    (list-synset-cuiset term-synset)))

(defn expand-custom-mrconso-records
  "Expand custom records where :str field is a structure rather than a
  string. "
  [record-list]
  (reduce (fn [newlist record]
            (if (coll? (:str record))
              (cond (= (-> record :str first) :preferred-name)
                    (conj newlist (assoc record :str (-> record :str second)))
                    (= (-> record :str first) :suppress-set) ; from lvg
                    (concat newlist
                            (map (fn [term]
                                   (assoc record :str (trim term) :sab "custom-lvg"))
                                 (-> record :str second)))
                    (= (-> record :str first) :termset)
                    (concat newlist
                            (map (fn [term]
                                   (assoc record :str (trim term) :sab "custom"))
                                 (-> record :str second)))
                    :else newlist)     ; if not recognized, don't add it.
              (conj newlist record)))
          '() record-list))


(defn expand-cui-concept-map
  "Expand custom records in cui-concept-map"
  [cui-concept-map]
  (into {}
        (map (fn [[cui recordlist]]
               (vector cui (expand-custom-mrconso-records recordlist)))
             cui-concept-map)))

(defn cui-concept-map-to-mrconso-write
  "Write cui-concept-map as a MRCONSO stream."
  ([wtr cui-concept-map]
   (synset/mrconso-from-cui-concept-map-write wtr cui-concept-map))
  ([wtr cui-concept-map term-synset]
   (synset/mrconso-from-cui-concept-map-write wtr cui-concept-map term-synset)))

(defn cuicoll-to-custom-mrsty-write
  [wtr cuicoll]
  (synset/custom-mrsty-write wtr (sort (into [] cuicoll))))


(defn cui-concept-map-to-mrconso-recordlist
  ([cui-concept-map]
   (synset/mrconso-records-from-cui-concept-map cui-concept-map))
  ([cui-concept-map term-synset]
   (synset/mrconso-records-from-cui-concept-map cui-concept-map term-synset)))

(defn stream-mrconso
  "Given termlist in params stream mrconso to standardOutput "
  [params]
  (let [{termlist "termlist"} params
        cui-concept-map (expand-cui-concept-map (termlist-to-cui-concept-map termlist))
        stream-mrconso (fn [out]
                         (dorun
                          (map #(do (.write ^Writer out ^String %)
                                    (.flush ^Writer out))
                               (cui-concept-map-to-mrconso-recordlist cui-concept-map))
                          (.flush ^Writer out)))]
    (piped-input-stream #(stream-mrconso (io/make-writer % {})))))

(defn cuicoll-to-custom-mrsty-recordlist
  [cuicoll]
  (synset/gen-custom-mrsty-recordlist (sort (into [] cuicoll))))
