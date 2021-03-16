(ns thermae.core
  (:gen-class)
  (:use hiccup.core)
  (:use hiccup.page)
  (:use slugger.core)
  (:use [clojure.string :only (blank? starts-with? replace)])
  (:use [clojure.pprint :only (pprint)])
  (:require
   [datascript.core :as d]
   [clojure.java.io :as io]
   [clojure.edn :as edn]
   [stasis.core :as stasis]
   [instaparse.core :as insta]
   ))

(def read-db #(edn/read-string {:readers d/data-readers} %))

(def db (->> "mikaelogy.edn" io/resource slurp read-db))

(def page-ids
  (flatten (vec (d/q '[:find ?x :where [?x :node/title]] db))))

(def page-entity
  (let [pull '[*]]
    (d/pull db pull 10170)))

(def everything
  (d/q '[
         :find (pull ?e [:node/title :block/string
                         :block/children {:block/children ...}])
         :where [?e :node/title]]
       db))

(def book-1
  (let [[[x]] (d/q '[
                     :find (pull ?e [:node/title
                                     :block/string
                                     :block/children {:block/children ...}])
                     :where [?e :node/title "books/Focusing"]]
                   db)]
    x))

(def foo
  '[
    :find (pull ?y [*])
    :where
    [?y :block/page ?page]
    (not [?y :block/heading])
    [?x :block/page ?page]
    [?x :block/refs ?attr]
    [?x :block/refs ?value]
    [?attr :node/title "Category"]
    [?value :node/title "books"]
    ])

(def markup-parser
  (insta/parser "
    root = attr / blockquote / thing

    blockquote = <'> '> thing+

    <thing> = (alias | xlink | link | img | text)+

    attr = #'[^:]+' <'::'> thing+

    alias = <'['> text <']'> <'((('> uid <')))'>
    xlink = <'['> text <']'> <'('> #'[^)]+' <')'>
    link = <'[['> thing <']]'>
    img = <'![]('> #'[^)]+' <')'>
    text = #'[^\\[\\]]+' | bracky
    bracky = <'['> #'[^\\[]' [thing] <']'> !'('
    uid = #'[a-zA-Z0-9]+'
  "))

(markup-parser "[hey ho wow](http://google.com)")

(defn tree-to-html [tree]
  (let [rules
        {
         :text str

         :root
         (fn [& x] (vec (concat [:div] x)))

         :link
         (fn [& x] [:a {:href "#" :class "ref"} x])

         :xlink
         (fn [x href] [:a {:href href} x])

         :img
         (fn [x] [:img {:src x}])

         :attr
         (fn [attr & xs] `[:span [:b ~attr] ": " ~@xs])
         }]
    (insta/transform rules tree)))

(defn process-body [x]
  (-> x
   (replace #" \(\[Location.*?\]\(.*?\)\)" "")
   (replace #"Highlights first synced by #Readwise (.*)" "$1 highlights:")
   (replace #"New highlights added (.*?) at .*" "$1 highlights:")))

(defn render-block [x indent]
  (let [body (x :block/string)]
    [:div {:class (if (starts-with? body "**Tags**") "hide" nil)
           :style (str "margin-left: " indent "em")}
     (vec (concat
           (if (not (blank? body))
             [:p (let [tree (markup-parser (process-body body))]
                   (if (insta/failure? tree)
                     (throw (ex-info "parse error" tree))
                     (tree-to-html
                      tree)))]
             [])
           (mapv #(render-block % (inc indent))
                 (x :block/children))))]))

;; (def all-pages-1
;;   (into {}
;;         (for [[node] everything]
;;           [(str "/" (->slug (node :node/title)) ".html")
;;            [[:meta {:charset "utf-8"}]
;;             [:link {:rel "stylesheet" :href "index.css"}]
;;             [:h1 (node :node/title)]
;;             (map render-block (node :block/children))]])))

(defn preify [x] [:pre (with-out-str (pprint (render-block x 0)))])

(def all-pages
  (into {}
        (for [[node] everything]
          [(str "/" (->slug (node :node/title)) ".html")
           (fn [context]
             (html5
              [:meta {:charset "utf-8"}]
              [:link {:rel "stylesheet" :href "index.css"}]
              [:h1 (node :node/title)]
              (map #(render-block % 0) (node :block/children))
              ))])))

(defn pages []
  (merge
   all-pages
   (stasis/slurp-directory "resources/static/" #".")
   {"/index.html"
    (html5
     [:meta {:charset "utf-8"}]
     [:link {:rel "stylesheet" :href "index.css"}]
     [:ul
      (map
       (fn [[x]] [:li [:a {:href (str "/" (->slug x) ".html")} x]])
       (sort (d/q '[:find ?t :where [_ :node/title ?t]] db)))])}))

(def app (stasis/serve-pages pages))

(defn -main
  "I don't do a whole lot ... yet..."
  [& args]

  (println (d/q '[:find ?t :where [_ :node/title ?t]] db))
  (println (html [:span "hey"]))
  )
