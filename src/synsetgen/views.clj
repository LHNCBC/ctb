(ns synsetgen.views
  (:require [clojure.string :refer [join]]
            [hiccup.core :refer [html]]
            [hiccup.page :refer [doctype include-css include-js xhtml xhtml-tag]]
            [hiccup.util]
            [ring.middleware.params :refer [wrap-params]]
            [synsetgen.umls-indexed :refer [get-preferred-name]]
            [synsetgen.process :refer [syntactically-simple? list-data-set-names]]))

(def ^:dynamic *front-page-title* "Home")

(defn gen-header-simple
  "Generate HTML page header"
  [title]
  [:head
   [:meta {:http-equiv "Content-type"
           :content "text/html; charset=utf-8"}]
   [:title title]
   (include-css "/css/base.css")
   (include-css "/css/style.css")
   [:style "em { color: red; }"]])

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

(defn view-layout
  "Base view layout."
  [title & content]
  (html
   (doctype :xhtml-strict)
   (xhtml-tag "en"
              (gen-header-simple title)
              [:body 
               [:div {:id "content"} 
                content
                (gen-footer title)
                ]])))

(defn expanded-termlist-review-page
  [synset]
  (view-layout "expanded termlist"
               (vec (concat [:table {:border "1"}]
                            (mapv (fn [[term cuimap]]
                                    (vector :tr
                                            (vec (concat (vector :td term)
                                                    (mapv #(vector :td %)
                                                          (keys cuimap))))))
                                  synset)))))



(defn termlist-submission-form
  [title]
  (view-layout
   title
   [:center
    [:h1 title]
    [:form {:method "post" :action "/processtermlist/"}
     [:textarea {:name "termlist" :rows 30 :cols 100}]
     ;; [:p [:input {:type "file" :value "termlistfile" :enctype "multipart/form-data"}]]
     [:p
      ;; [:input.action {:type "submit" :value "test0" :name "cmd"}]
      ;; [:input.action {:type "submit" :value "test1" :name "cmd"}]
      ;; [:input.action {:type "submit" :value "test->cui" :name "cmd"}]
      ;; [:input.action {:type "submit" :value "synset table" :name "cmd"}]
      [:input.action {:type "submit" :value "synset list" :name "cmd"}]]
     [:p (vec (concat [:select]
                      (mapv #(vector :option {:value %} %)
                            (list-data-set-names))))]
     ]]))

(defn display-termlist
  [termlist]
  (view-layout "termlist"
               (vec (concat [:ul] (mapv #(vector :li %) termlist)))))

(defn term-cui-mapping-page
  [synset]
  (view-layout "expanded termlist"
               (vec (concat [:table {:border "1"}]
                            (mapv (fn [[term cuimap]]
                                    (vector :tr
                                            (vec (concat (vector :td term)
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
  [synset]
  (vec
   (concat
    [:table {:border "1"}]
    (mapv (fn [[term cuimap]]
            [:tr [:th term]
             [:td (vec (concat [:table {:border "1"}]
                               (mapv (fn [[cui termlist]]
                                       [:tr [:th cui]
                                        [:td (vec (concat [:table {:border "1"}]
                                                          (if (empty? termlist)
                                                            [:tr [:td "<empty>"]]
                                                            (mapv (fn [candidate-term]
                                                                    [:tr [:td candidate-term]])
                                                                  termlist))))]])
                                     cuimap)))]])
                     synset))))

(defn synset-table-page
  [synset]
  (view-layout
   "SynSet View"
   (nested-synset-tables synset)))

(defn gen-header-jquery
  "Generate HTML page header with JQuery elements"
  [title]
  [:head
   [:meta {:http-equiv "Content-type"
           :content "text/html; charset=utf-8"}]
   [:title title]
   (include-css "/css/base.css")
   (include-css "/css/style.css")
   [:style "em { color: red; }"]
   (include-js "/js/jquery-1.4.2.min.js")
   (include-js "/js/jquery.collapsibleCheckboxTree.js")
   (include-css "/css/jquery.collapsibleCheckboxTree.css")
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
  [title & content]
  (html
   (doctype :xhtml-strict)
   (xhtml-tag "en"
              (gen-header-jquery title)
              [:body
               [:div {:id "content"} 
                content
                (gen-footer title)
                ]])))

(defn nested-synset-lists
  [synset]
  ;; [:div {:id "container"}
   [:form {:class "list" :method "post" :action "/processfiltertermlist/"}
    (vec
     (concat
      [:ul {:id "example"}]
      (mapv (fn [[term cuimap]]
              [:li term
               (vec (concat [:ul]
                            (mapv (fn [[cui conceptinfo]]
                                    [:li  [:input {:type "checkbox" :checked "checked"}] (str (:preferred-name conceptinfo) " (" cui ")" )
                                     (vec (concat [:ul]
                                                  (if (empty? (:termset conceptinfo))
                                                    [:li "<empty>"]
                                                    (mapv (fn [candidate-term]
                                                            [:li [:input (merge {:type "checkbox"
                                                                                 :name (str term "|" cui "|" candidate-term)}
                                                                                (when (syntactically-simple? candidate-term)
                                                                                  {:checked "checked"}))]
                                                             candidate-term])
                                                          (:termset conceptinfo)))))])
                                  cuimap)))])
            (into (sorted-map) synset))))
    [:input.action {:type "submit" :value "submit" :name "submit"}]]
;;]
)

(defn synset-list-page
  [synset]
  (view-layout-jquery
   "SynSet View"
   [:h1 "SynSet View"]
   (nested-synset-lists synset)))

   


(defn filtered-termlist-view
  [req]
  (view-layout "Filtered TermList"
               [:h1 "Filtered Termlist has been processed"] 
               [:h2 "MRCONSO.RRF"]
               [:p "The file " [:a {:href "/output/mrconso.rrf"} [:code "mrconso.rrf"]] 
                " has been created.  Click the "
                [:a {:href "/output/mrconso.rrf"} "link"] " to download it. "]
                [:p "Semantic type file:    " [:a {:href "/output/mrsty.rrf"} "mrsty.rrf"] ]
                [:p "Source attribute file: " [:a {:href "/output/mrsat.rrf"} "mrsat.rrf"] ]
               [:h2 "Filtered TermList"]
               (vec (concat [:ul ]
                            (mapv (fn [[k v]]
                                    [:li (str k " -> " v)])
                                  (sort
                                   (dissoc (:params req) "submit")))))
               ))


