(ns thermae.core
  (:gen-class)
  (:use hiccup.core)
  (:use hiccup.page)
  (:use slugger.core)
  (:use [clojure.string :only (blank? starts-with?)])
  (:use [clojure.pprint :only (pprint)])
  (:use  [clojure.core.match :only [match]])

  (:require
   [clojure.string :as s]
   [datascript.core :as d]
   [clojure.java.io :as io]
   [clojure.edn :as edn]
   [stasis.core :as stasis]
   [instaparse.core :as insta]
   ))

(defn slug [title]
  (s/replace (->slug title) "-slash-" "/"))

(def read-db #(edn/read-string {:readers d/data-readers} %))

(def db (->> "mikaelogy.edn" io/resource slurp read-db))

(def page-ids
  (flatten (vec (d/q '[:find ?x :where [?x :node/title]] db))))

(def page-entity
  (let [pull '[*]]
    (d/pull db pull 10170)))

(def everything
  (d/q '[
         :find (pull ?e [:node/title
                         :block/uid
                         :block/string
                         :block/order
                         :block/heading
                         :block/children {:block/children ...}])
         :where [?e :node/title]]
       db))

(defn find-node [title]
  (let [it
        (d/q '[
               :find (pull ?e [:node/title
                               :block/uid
                               :block/string
                               :block/order
                               :block/heading
                               :block/children {:block/children ...}])
               :in $ ?title
               :where [?e :node/title ?title]]
             db title)]
    (first (first it))))

(defn find-block [uid]
  (let [it
        (d/q '[
               :find (pull ?e [:block/string
                               {:block/page [:node/title]}
                               :block/children {:block/children ...}])
               :in $ ?uid
               :where [?e :block/uid ?uid]]
             db uid)]
    (first (first it))))

(def book-focusing (find-node "books/Focusing"))
(def blog-1 (find-node "March 11th, 2021"))

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
    root = attr | blockquote | !'> ' paragraph

    attr       = word <':: '> inline
    blockquote = <'> '> inline
    paragraph  = inline

    inline = (blockref | alias | xlink | link | img | text)+

    word   = #'[^\\*\\[\\]\\(\\)\\_:]+'
    symbol =  #'[\\*\\[\\]\\(\\)\\_:]'

    uid = #'[a-zA-Z0-9-_]+'

    <text> = (word | italic | bold) / symbol

    blockref = <'(('> uid <'))'>
    alias = <'['> text <']'> <'((('> uid <')))'>
    xlink = <'['> text <']'> <'('> #'[^)]+' <')'>
    link  = <'[['> inline <']]'>
    img   = <'![]('> #'[^)]+' <')'>

    italic = <'__'> (!'__' text) <'__'>
    bold   = <'**'> (!'__' text) <'**'>
  "))

(markup-parser "foo:: hello")

(markup-parser "> Focusing applies to more than personal problems. Creativity, originality and depth require something like focusing in any field: the capacity to attend to what is not yet verbalized. ([Location 2977](https://readwise.io/to_kindle?action=open&asin=B004CLYCSO&location=2977))")

(markup-parser "> It is good to love as many things as one can, for therein lies true strength, and those who love much, do much and accomplish much, and whatever is done with love is done well. [*](((ftG7SOukS)))")

(markup-parser "> To lure back these Homeric gods is a saving possibility after the death of God: it would allow us to survive the breakdown of monotheism while resisting the descent into a nihilistic existence. [*](((nvhXa_-Ve)))")

(markup-parser "> Talk of efficiency and sustainability are simply artifacts of the relentless use of fossil fuels. In a solar economy, you could have a disco in every single room of your house and way fewer lifeforms would suffer, perhaps vanishingly few, compared to the act of simply turning on the lights in an oil economy. You could have strobes and decks and lasers all day and night to your heart’s content. [*](((j0n9WOj6-)))")

(defn transform-tree-to-link [tree]
  (let [go transform-tree-to-link]
    (match tree
      [:inline x] (go x)
      [:word x] x
      _ (throw (ex-info "transform-tree-to-link" {:tree tree})))))

(defn transform-tree [tree]
  (let [go transform-tree]
    (match tree
      [:root x] (go x)

      [:attr x y]
      (list [:b (go x)] ": " (go y))

      [:text x] [:span (go x)]
      [:word x] x
      [:italic x] [:em (go x)]

      [:paragraph x] (go x)

      [:inline & xs] (map go xs)

      [:link x] [:a.ref {:href (transform-tree-to-link x)} (go x)]

      [:blockquote x] [:blockquote (go x)]
      [:symbol x] x

      [:alias [:symbol "*"] [:uid y]]
      (let [it (find-block y)]
        [:a.cite {:href "#"} "*"])

      [:blockref [:uid x]]
      (let [it (find-block x)
            title (-> it :block/page :node/title)]
        [:a {:href (str "/" (slug title))} title])

      _ (throw (ex-info "transform error" {:tree tree}))
      ))
  )

(defn tree-to-html [tree]
  (transform-tree tree))

(defn process-body [x]
  (-> x
   (s/replace #" \(\[Location.*?\]\(.*?\)\)" "")
   (s/replace #"Highlights first synced by #Readwise (.*)" "$1 highlights:")
   (s/replace #"New highlights added (.*?) at .*" "$1 highlights:")))

(defn render-body [body indent heading]
  (let [tree (markup-parser (process-body body))]
    (if (insta/failure? tree)
      (throw (ex-info "parse error" tree))
      (let [html (tree-to-html tree)
            style {}]
        (match html
          [:blockquote x]
          [:blockquote style x]

          xs (if heading
               [(keyword (str "h" heading)) style html]
               [:p style html]))))))

(defn render-block [x indent]
  (let [body (x :block/string)
        kids (map #(render-block % (inc indent))
                  (sort-by :block/order (x :block/children)))]
    (if (blank? body)
      kids
      (if (starts-with? body "**Tags**")
        ()
        (list (render-body body indent (:block/heading x))
              [:div.indent
               kids])))))

(defn preify [x] [:pre (with-out-str (pprint (render-block x 0)))])

(defn single-page [node]
  (println "Rendering" (:block/uid node) (:node/title node))
  (html5
   [:meta {:charset "utf-8"}]
   [:link {:rel "stylesheet" :href "/index.css"}]
   [:header
    "@meekaale"
    " » "
    "Notes"
    " » "
    [:strong (node :node/title)]
    ]
   [:article
    [:h1 (node :node/title)]
    (map #(render-block % 0) (node :block/children))]
   ))

(defn test-1 []
  (pprint (single-page (find-block "f4a5OO7AE"))))

(defn test-2 []
  (pprint (single-page (find-node "March 10th, 2021"))))

(def all-pages
  (into {}
        (for [[node] everything]
          [(str "/" (slug (node :node/title)) ".html")
           (fn [context]
             (single-page node))])))

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
       (fn [[x]] [:li [:a {:href (str "/" (slug x) ".html")} x]])
       (sort (d/q '[:find ?t :where [_ :node/title ?t]] db)))])}))

(def app (stasis/serve-pages pages))

(defn -main
  "I don't do a whole lot ... yet..."
  [& args]

  (println (d/q '[:find ?t :where [_ :node/title ?t]] db))
  (println (html [:span "hey"]))
  )
