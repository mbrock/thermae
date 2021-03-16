(ns thermae.core
  (:gen-class)
  (:use hiccup.core)
  (:use hiccup.page)
  (:use slugger.core)
  (:use [clojure.string :only (blank?)])
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
                     :where [?e :node/title "books/Prayer"]]
                   db)]
    x))

(def markup-parser
  (insta/parser "
    root = attr / thing

    <thing> = (alias | xlink | link | img | text)+

    attr = #'[^:]+' '::' thing+

    alias = <'['> text <']'> <'((('> uid <')))'>
    xlink = <'['> text <']'> <'('> #'[^)]+' <')'>
    link = <'[['> thing <']]'>
    img = <'![]('> #'[^)]+' <')'>
    text = #'[^\\[\\]]+' | bracky
    bracky = <'['> #'[^\\[]' [thing] <']'> !'('
    uid = #'[a-zA-Z0-9]+'
  "))

(defn tree-to-html [tree]
  (let [rules
        {:root (fn [& x] (vec (concat [:div] x)))
         :text str
         :link (fn [x] [:a {:href "#" :class "link"} x])
         :img (fn [x] [:img {:src x}])
         }]
    (insta/transform rules tree)))

(defn render-block [x]
  (let [body (x :block/string)]
    [:div
     (vec (concat
           (if (not (blank? body))
             [:p (let [tree (markup-parser body)]
                   (if (insta/failure? tree)
                     (throw (ex-info "parse error" tree))
                     (tree-to-html
                      tree)))]
             [])
           (mapv render-block (x :block/children))))]))

;; (def all-pages-1
;;   (into {}
;;         (for [[node] everything]
;;           [(str "/" (->slug (node :node/title)) ".html")
;;            [[:meta {:charset "utf-8"}]
;;             [:link {:rel "stylesheet" :href "index.css"}]
;;             [:h1 (node :node/title)]
;;             (map render-block (node :block/children))]])))

(defn preify [x] [:pre (with-out-str (pprint (render-block x)))])

(def all-pages
  (into {}
        (for [[node] everything]
          [(str "/" (->slug (node :node/title)) ".html")
           (fn [context]
             (html5
              [:meta {:charset "utf-8"}]
              [:link {:rel "stylesheet" :href "index.css"}]
              [:h1 (node :node/title)]
              (map render-block (node :block/children))
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
