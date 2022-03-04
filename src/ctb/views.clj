(ns ctb.views
  (:require [clojure.string :refer [join]]
            [clojure.java.io :as io]
            [hiccup.core :refer [html]]
            [hiccup.page :refer [doctype include-css include-js xhtml xhtml-tag]]
            [hiccup.util]
            [ctb.umls-indexed :refer [get-preferred-name]]
            [ctb.process :refer [list-data-set-names
                                 sanitize-params
                                 render-path]])
  (:import (org.owasp.encoder Encode))
  (:gen-class))

;; # HTML Views
;;
(def ^{:dynamic true} *header-color*         "#CAE1F9")
(def ^{:dynamic true} *data-color*           "#E5E4E2")
(def ^{:dynamic true} *highlight-data-color* "#A9F5A9")
(def ^:dynamic *front-page-title* "Home")

(defn gen-header-simple
  "Generate HTML page header"
  [request title]
  [:head
   [:meta {:http-equiv "Content-type"
           :content "text/html; charset=utf-8"}]
   [:title title]
   (include-css (str (:context request) "/css/base.css"))
   (include-css (str (:context request) "/css/style.css"))])

(defn gen-footer
  "Generate HTML footer for page, make front page link live if not at
  frontpage.  Note: this should probably use the current URL to
  determine if home page link should live."
  [request title]
  [:div {:id "footer"}
   [:address
    (if (= title *front-page-title*)
      *front-page-title*
      [:a {:href (str (:context request) "/")} *front-page-title*]) " | "
    [:a {:href "http://ii.nlm.nih.gov"} "Indexing Initiative"]]])

(defn apply-banner-static
  "Apply banner if present in properties."
  [request]
  [:p {:style "border-style:solid;border-width;1px;border-color:red;padding:2px;font-size:17px;"}
   "Because of a lapse in government funding, the information on this
website may not be up to date, transactions submitted via the website
may not be processed, and the agency may not be able to respond to
inquiries until appropriations are enacted.  The NIH Clinical Center
(the research hospital of NIH) is open. For more details about its
operating status, please
visit " [:a {:href "https://cc.nih.gov/"} "cc.nih.gov"] ".  Updates regarding
government operating status and resumption of normal operations can be
found at " [:a {:href "https://USA.gov/"} "USA.gov"] "."])

(defn apply-banner
  [request]
  (if (.exists (io/file "banner.html"))
    (slurp "banner.html")
    [:br]))

(defn view-layout
  "Base view layout."
  [request title & content]
  (html
   (doctype :xhtml-strict)
   (xhtml-tag "en"
              (gen-header-simple request title)
              [:body
               (apply-banner request)
               [:div {:id "content"} 
                content
                (gen-footer title)
                ]])))

(defn expanded-termlist-review-page
  "Generate Termlist Review page view"
  [request synset]
  (view-layout
   request "expanded termlist"
   (vec
    (concat [:table {:border "1"}]
            (mapv (fn [[term cuimap]]
                    (vector :tr
                            (vec (concat (vector :td term)
                                         (mapv #(vector :td (Encode/forCDATA %))
                                               (keys cuimap))))))
                  synset)))))

(defn termlist-submission-form
  "Generate Termlist submission form page view"
  [request title]
  (view-layout
   request title
   [:div {:id "panel"}
    [:div {:id "formcontent"}
     [:h1 title]
     [:p "Enter terms, one term per line."]
     [:form {:method "post" :action (str (:context request) "/processtermlist/")}
      [:textarea {:name "termlist" :rows 30 :cols 100}]
      ;; [:p [:input {:type "file" :value "termlistfile" :enctype "multipart/form-data"}]]
      [:p
       ;; [:input.action {:type "submit" :value "test0" :name "cmd"}]
       ;; [:input.action {:type "submit" :value "test1" :name "cmd"}]
       ;; [:input.action {:type "submit" :value "test->cui" :name "cmd"}]
       ;; [:input.action {:type "submit" :value "synset table" :name "cmd"}]
       [:input.action {:type "submit" :value "submit" :name "cmd"}]]
      (comment 
        [:p (vec (concat [:select]
                         (mapv #(vector :option {:name "dataset" :value %} %)
                               (list-data-set-names))))])
      ]]
    [:h2 "Example Terms"]
    [:ul
     [:li "lumbar disk"]
     [:li "low back pain"]
     [:li "pneumonia"]
     [:li "rlq abd pain"]
     [:li "rlq abd tenderness"]]]))

(defn display-termlist
  "Generate Termlist View." 
  [request termlist]
  (view-layout
   request "termlist"
   (vec (concat [:ul] (mapv #(vector :li (Encode/forCDATA %))
                            termlist)))))

(defn term-cui-mapping-page
  "Generate Term -> CUI mapping page view" 
  [request synset]
  (view-layout
   request "expanded termlist"
   (vec
    (concat
     [:table {:border "1"}]
     (mapv (fn [[term cuimap]]
             (vector :tr
                     (vec (concat (vector :td (Encode/forCDATA term))
                                  (mapv #(vector :td %)
                                        (keys cuimap))))))
           synset)))))

(defn gen-footer
  "Generate HTML footer for page, make front page link live if not at
  frontpage.  Note: this should probably use the current URL to
  determine if home page link should live."
  [title]
  [:div {:id "footer"}
   [:address
    (if (= title *front-page-title*)
      *front-page-title*
      [:a {:href "/"} *front-page-title*]) " | "
    [:a {:href "http://ii.nlm.nih.gov"} "Indexing Initiative"]]])


(defn nested-synset-tables
  "Generate Synonym Set Table view"  
  [synset]
  (vec
   (concat
    [:table {:border "1"}]
    (mapv (fn [[term cuimap]]
            [:tr
             [:th (Encode/forCDATA term)]
             [:td (vec
                   (concat
                    [:table {:border "1"}]
                    (mapv (fn [[cui termlist]]
                            [:tr
                             [:th (Encode/forCDATA cui)]
                             [:td (vec
                                   (concat
                                    [:table {:border "1"}]
                                    (if (empty? termlist)
                                      [:tr [:td "<empty>"]]
                                      (mapv (fn [candidate-term]
                                              [:tr
                                               [:td
                                                (Encode/forCDATA candidate-term)]])
                                            termlist))))]])
                          cuimap)))]])
          synset))))

(defn synset-table-page
  "Generate Synonym Set Table Page View"  
  [request synset]
  (view-layout
   request "SynSet View"
   (nested-synset-tables synset)))

(defn gen-header-jquery
  "Generate HTML page header with JQuery elements"
  [request title]
  [:head
   [:meta {:http-equiv "Content-type"
           :content "text/html; charset=utf-8"}]
   [:title title]
   (include-css (str (:context request) "/css/base.css"))
   (include-css (str (:context request) "/css/style.css"))
   (include-js (str (:context request) "/js/jquery-3.6.0.min.js"))
   (include-js (str (:context request) "/js/jquery.collapsibleCheckboxTree.js"))
   (include-css (str (:context request) "/css/jquery.collapsibleCheckboxTree.css"))
   [:script {:type "text/javascript"}
    "
jQuery(document).ready(function(){
		$('ul#example').collapsibleCheckboxTree();
});
/*
jQuery(document).ready(function(){
		$('ul#example').collapsibleCheckboxTree({
		checkParents : true, // When checking a box, all parents are checked (Default: true)
		checkChildren : false, // When checking a box, all children are checked (Default: false)
		shiftClickEffectChildren : true, // When shift-clicking a box, all children are checked or unchecked (Default: true)
		uncheckChildren : true, // When unchecking a box, all children are unchecked (Default: true)
		includeButtons : true, // Include buttons to expand or collapse all above list (Default: true)
		initialState : 'default' // Options - 'expand' (fully expanded), 'collapse' (fully collapsed) or default
												});
});
*/
"]])

(defn view-layout-jquery
  "Base view layout."
  [request title & content]
  (html
   (doctype :xhtml-strict)
   (xhtml-tag "en"
              (gen-header-jquery request title)
              [:body
               [:div {:id "content"} 
                content
                (gen-footer title)
                ]])))

(defn nested-synset-lists
  "Generate Synonym Set Nested Tree List with Term selection
  buttons. "
  [request synset]
  [:div {:id "container"}
   [:form {:class "list" :method "post"
           :action (str (:context request) "/processfiltertermlist/")}
    (vec
     (concat
      [:ul {:id "example"}]
      (mapv (fn [[term cuimap]]
              [:li term
               (vec (concat [:ul]
                            (mapv (fn [[cui conceptinfo]]
                                    [:li  [:input {:type "checkbox" :checked "checked"}] (str (:preferred-name conceptinfo))
                                     " " [:em "(" cui ")"] 
                                     (vec (concat [:ul]
                                                  (if (empty? (:termset conceptinfo))
                                                    [:li "<empty>"]
                                                    (mapv (fn [^String candidate-term]
                                                            [:li [:input (merge {:type "checkbox"
                                                                                 :name (str term "|" cui "|" candidate-term)}
                                                                                {}
                                                                                 (if (contains? (:suppress-set conceptinfo) candidate-term)
                                                                                   {}
                                                                                   {:checked "checked"}) )]
                                                             candidate-term])
                                                          (sort (into [] (conj
                                                                          (if (empty? (:termset conceptinfo))
                                                                            (vector term)
                                                                            (:termset conceptinfo))
                                                                          term)))))))])
                                  cuimap)))])
            (into (sorted-map) synset))))
    [:p [:input.action {:type "submit" :value "submit" :name "submit"}]]]
   ]
  )

(defn synset-list-page
  "Generate Synonym Set Editing View"
  [request synset]
  (view-layout-jquery
   request "SynSet View"
   [:h1 "SynSet View"]
   [:div
    [:ul
     [:li "When checking a box, all parents are checked."]
     [:li "When shift-clicking a box, all children are checked or unchecked."]
     [:li "When unchecking a box, all children are unchecked."]]
    (nested-synset-lists request synset)
    ]))

(defn filtered-termlist-view
  "View generated after processing termlist filtered by user."
  [request]
  (let [params (sanitize-params (:params request))
        user (-> request :cookies (get "termtool-user") :value) ; get :user values from cookie part of request
        dataset (-> request :session :dataset) ; Get :dataset from :session part of request.
        workurl (if (:context request)
                  (format "%s/dataset/%s" (:context request) dataset)
                  (format "/dataset/%s" dataset))]
    (view-layout request "Filtered TermList"
                 [:h1 "Filtered Termlist has been processed"] 
                 [:h2 "MRCONSO.RRF"]
                 [:p "The file "
                  [:a {:href (str workurl "/mrconso.rrf")}
                   [:code "mrconso.rrf"]]
                  " has been created.  Click the "
                  [:a {:href (str workurl "/mrconso.rrf")} "link"]
                  " to download it. "]
                 [:h2 "MRSTY.RRF"]
                 [:p "Semantic type file:    "
                  [:a {:href (str workurl "/mrsty.rrf")}
                   [:code "mrsty.rrf"]]]
                 [:h2 "Filtered TermList"]
                 (vec (concat [:ul ]
                              (mapv (fn [[k v]]
                                      [:li (Encode/forHtmlContent (str  k " -> " v))])
                                    (sort
                                     (dissoc (:params request) "submit")))))
                 [:p
                  [:a {:href (str (:context request) "/datasetsinfo/")}
                   "Current Datasets"]]
               )))

(defn display-error-message
  "Generate HTML page for error message"
  [request message]
  (view-layout request message
               [:h1 "Error"]
               [:p [:strong (Encode/forCDATA message)]]))

(defn display-dataset-list
  "Generate HTML page including dataset list for user. "
  [request username dataset-map]
  (let [workurl (if (:context request)
                  (format "%s/dataset" (Encode/forCDATA (:context request)))
                  "/dataset")
        dataset-rendering (mapv
                           (fn [[dataset filenames]]
                             (vector
                              :li (Encode/forCDATA dataset)
                              (vec
                               (concat
                                [:ul]
                                (mapv
                                 #(vector
                                   :li
                                   [:a {:href
                                        (format "%s/%s/%s"
                                                (Encode/forCDATA workurl)
                                                (Encode/forCDATA dataset) %)}
                                    %])
                                 filenames)))))
              dataset-map)]
    (view-layout request (str "DataSet list for " (Encode/forCDATA username))
                 [:h1 (str "DataSet list for user " (Encode/forCDATA username))]
                 (vec (concat [:ui] dataset-rendering))
               )))


(defn user-error-message
  "Generate user error message page view"
  [request title message]
  (view-layout
   request title
   [:div {:id "panel"}
    [:h1 (Encode/forCDATA title)]
    [:p [:strong (Encode/forCDATA message)]]]))
