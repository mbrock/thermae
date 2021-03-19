(ns thermae.core
  (:gen-class)
  (:use hiccup.core)
  (:use hiccup.page)
  (:use slugger.core)
  (:use [clojure.pprint :only (pprint)])
  (:use  [clojure.core.match :only [match]])

  (:require
   [clojure.string :as s]
   [clojure.data.json :as json]
   [datascript.core :as d]
   [clojure.java.io :as io]
   [clojure.edn :as edn]
   [stasis.core :as stasis]
   [instaparse.core :as insta]))

(def read-db #(edn/read-string {:readers d/data-readers} %))
(def db (->> "mikaelogy.edn" io/resource slurp read-db))

(defn page-type [title]
  (if (re-matches #"[A-Z].*? [0-9]+(th|nd|st|rd), [0-9]{4}" title)
    {:type :daily-note :href (str "days/" (->slug title))}
    (match (re-matches #"\[\[(.*?)\]\] (.*)" title)
      [_ "books" x]    {:type :book :href (str "books/" (->slug x))}
      [_ "articles" x] {:type :article :href (str "articles/" (->slug x))}
      _ {:type :topic :href (str "topics/" (->slug title))}
      )))

(defn process-body [x]
  (-> x
      (s/replace #"\{\{word-count\}\}" "")
      (s/replace #" \(\[Location.*?\]\(.*?\)\)" "")
      (s/replace #"Highlights first synced by #Readwise (.*)" "Highlights from $1.")
      (s/replace #"New highlights added (.*?) at .*" "Highlights from $1.")))

(defn slug [title]
  (:href (page-type title)))

(def page-ids
  (flatten (vec (d/q '[:find ?x :where [?x :node/title]] db))))

(def page-entity
  (let [pull '[*]]
    (d/pull db pull 10170)))

(defn query [q & xs] (apply d/q (concat (list q db) xs)))

(def page-pull
  '[:node/title
    :block/uid
    :block/string
    :block/order
    :block/heading
    {:block/refs [:node/title]}
    {:block/_refs
     [:block/uid :block/string
      {:block/page [:node/title]}]}
    :block/children
    {:block/children ...}])

(def everything
  (map first
       (query
        '[:find (pull ?e pattern)
          :in $ pattern
          :where [?e :node/title]]
        page-pull)))

(def nodes-by-title (into {} (for [x everything] [(:node/title x) x])))

(def all-books
  (map #(nodes-by-title %)
       (sort
        (query '[:find [?x ...] :where [?n :node/title ?x] [?n :block/refs ?r] [?r :node/title "books"]]))))

(def all-articles
  (map #(nodes-by-title %)
       (sort
        (query '[:find [?x ...] :where [?n :node/title ?x] [?n :block/refs ?r] [?r :node/title "articles"]]))))

(def some-book
  (nodes-by-title "[[books]] Good Clean Fun"))

(defn page-author-markup [page]
  (s/replace
   (:block/string
    (first
     (filter #(s/starts-with? (:block/string %) "Author:: ")
       (:block/children page))))
   #"^Author:: " ""))

(def daily-notes
  (reverse
   (sort-by
       #(.parse (java.text.SimpleDateFormat. "MM-dd-yyy")
                (first %))
     (query
      '[
        :find ?uid ?date ?title
        :where
        [?page :block/uid ?uid]
        [?x :block/page ?page]
        [?page :node/title ?date]
        [?x :block/refs ?attr]
        [?x :block/refs ?value]
        [?x :block/string ?title]
        [?attr :node/title "Note"]
        ]
      )))
  )

(defn find-node [title]
  (let [it
        (d/q '[
               :find (pull ?e [:node/title
                               :block/uid
                               :block/string
                               :block/order
                               :block/heading
                               :block/refs {:block/refs [*]}
                               {:block/_refs [:block/uid :block/string {:block/page [:node/title]}]}
                                :block/children {:block/children ...}])
               :in $ ?title
               :where [?e :node/title ?title]]
             db title)]
    (first (first it))))

(def ancestor-rule '[[(ancestor ?b ?a)
                      [?a :block/children ?b]]
                     [(ancestor ?b ?a)
                      [?parent :block/children ?b]
                      (ancestor ?parent ?a)]])

(defn block-page [uid]
  (query '[:find ?title . :in $ ?uid %
           :where
           [?block :block/uid ?uid]
           [?page :node/title ?title]
           (ancestor ?block ?page)
           ]
         uid ancestor-rule))

(def orphans
  (query '[:find ?x ?parent
           :where
           [?x :block/string]
           (not [?x :block/parents])
           (not [?x :node/title])
           [?parent :block/children ?x]
           ]))

(def parentless
  (query '[:find ?x ?uid ?order ?s :where
           [?x :block/string ?s]
           [?x :block/order ?order]
           [?x :block/uid ?uid]
           (not [?x :block/parents])
           (not [?x :node/title])
           ]))

(defn find-parent [id]
  (query '[:find ?uid . :in $ ?id :where
           [?y :block/uid ?uid]
           [?y :block/children ?id]] id))

(defn find-parents [id]
  (if-let [x (find-parent id)]
    (conj (find-parents x) x)
    []))

(defn hmm []
  (filter identity
    (for [[x uid order s] parentless]
      (let [parents (reverse (find-parents x))]
        (if (seq parents) {:uid uid :parent (first parents) :order order :text s})))))

(defn find-block [uid]
  (let [it
        (d/q '[
               :find (pull ?e [:block/string
                               :db/id
                               :block/page
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

    inline = (blockref | alias | xlink | link | img | video | text)+

    word   = #'[^\\*\\[\\]\\(\\)\\_:{}]+'
    symbol =  #'[\\*\\[\\]\\(\\)\\_:{}]'

    uid = #'[a-zA-Z0-9-_]+'

    <text> = (word | italic | bold) / symbol

    blockref = <'(('> uid <'))'>
    alias = <'['> text <']'> <'((('> uid <')))'>
    xlink = <'['> text <']'> <'('> #'[^)]+' <')'>
    link  = <'[['> inline <']]'>
    img   = <'![]('> #'[^)]+' <')'>

    italic = <'__'> (!'__' text) <'__'>
    bold   = <'**'> (!'__' text) <'**'>

    video = <'{{[[video]]: '> #'[^}]+' <'}}'>
  "))

(defn test-1 []
  (markup-parser "foo:: hello")

  (markup-parser "> Focusing applies to more than personal problems. Creativity, originality and depth require something like focusing in any field: the capacity to attend to what is not yet verbalized. ([Location 2977](https://readwise.io/to_kindle?action=open&asin=B004CLYCSO&location=2977))")

  (markup-parser "> It is good to love as many things as one can, for therein lies true strength, and those who love much, do much and accomplish much, and whatever is done with love is done well. [*](((ftG7SOukS)))")

  (markup-parser "> To lure back these Homeric gods is a saving possibility after the death of God: it would allow us to survive the breakdown of monotheism while resisting the descent into a nihilistic existence. [*](((nvhXa_-Ve)))")

  (markup-parser "> Talk of efficiency and sustainability are simply artifacts of the relentless use of fossil fuels. In a solar economy, you could have a disco in every single room of your house and way fewer lifeforms would suffer, perhaps vanishingly few, compared to the act of simply turning on the lights in an oil economy. You could have strobes and decks and lasers all day and night to your heart’s content. [*](((j0n9WOj6-)))"))

(defn transform-tree-to-link [tree]
  (let [go transform-tree-to-link]
    (match tree
      [:inline & xs] (apply str (map go xs))
      [:word x] x
      [:symbol x] x
      [:link x] (str "[[" (go x) "]]")
      _ (throw (ex-info "transform-tree-to-link" {:tree tree})))))

(defn namespaced-title? [x]
  (re-matches #".+/.+" x))

(defn link-href [x]
  (str "/" (slug (transform-tree-to-link x))))

(declare transform-tree)

(defn render-inline
  ([body] (render-inline body false))
  ([body no-links?]
   (let [tree (markup-parser (process-body body))]
     (if (insta/failure? tree)
       (throw (ex-info "parse error" tree))
       (let [html (transform-tree tree no-links?)
             style {}]
         (match html
           [:blockquote x] x
           x x))))))

(defn transform-tree [tree no-links?]
  (let [go #(transform-tree % no-links?)
        go-without-links #(transform-tree % true)]
    (match tree
      [:root x] (go x)

      [:attr x y]
      (list [:strong (go x)] ": " (go y))

      [:text x] [:span (go x)]
      [:word x] x
      [:italic x] [:em (go x)]
      [:bold x] [:strong (go x)]

      [:paragraph x] (go x)

      [:inline
       [:word "https"]
       [:symbol ":"]
       [:word x]]
      (let [url (str "https:" x)]
        (if no-links? url
            [:a {:href url} url]))

      [:inline & xs] (map go xs)

      [:link ([:inline [:word (title :guard namespaced-title?)]] :as x)]
      [:a.ref.ns {:href (link-href x)}
       (render-inline title)]

      [:link [:inline [:word "books"]]]
      (if no-links? ""
          [:a.ref {:href "/books"} "📙"])

      [:link [:inline [:word "articles"]]]
      (if no-links? ""
          [:a.ref {:href "/articles"} "📰"])

      [:link x]
      (if no-links? (go x)
          [:a.ref {:href (link-href x)}
           (go-without-links x)])

      [:img x] [:img {:src x}]

      [:blockquote x] [:blockquote (go x)]
      [:symbol x] x

      [:alias [:symbol "*"] [:uid y]]
      (let [it (find-block y)]
        (if no-links? "*"
            [:a.cite {:href "#"} "*"]))

      [:blockref [:uid x]]
      (let [it (find-block x)
            title (-> it :block/page :node/title)
            content (:block/string it)
            name (go-without-links (markup-parser content))]
        (if no-links? name
            [:a {:href (str "/" (slug title))} name]))

      [:xlink text url]
      (if no-links? (go text)
          [:a {:href url} (go-without-links text)])

      [:video url]
      (if-let [[_ id] (re-find #"\?v=([^&]+)" url)]
        [:iframe
         {:height 360 :src (str "https://www.youtube.com/embed/" id)}]
        [:span "hmm " url]
        )
      _ (throw (ex-info "transform error" {:tree tree}))
      ))
  )

(defn tree-to-html [tree]
  (transform-tree tree false))

(defn render-body
  ([body] (render-body body 0 nil))
  ([body indent heading]
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
                [:p style html])))))))

(defn prerender-block [x]
  (let [body (x :block/string)
        kids (map prerender-block (x :block/children))]
    (if (s/blank? body)
      kids
      (list (markup-parser (process-body body)) [:div.indent kids]))))

(defn render-block [x indent]
  (let [body (x :block/string)
        kids (map #(render-block % (inc indent))
                  (sort-by :block/order (x :block/children)))]
    (if (s/blank? body)
      kids
      (if (s/starts-with? body "**Tags**")
        ()
        (list (render-body body indent (:block/heading x))
              [:div.indent
               kids])))))

(defn preify [x] [:pre (with-out-str (pprint (render-block x 0)))])

(defn render-refs [title xs]
  [:li
   [:div.linkref
    [:h3 [:a {:href (str "/" (slug title))} (render-inline title true)]]
    [:div
     (map (fn [x]
            (render-body (:block/string x) 0 nil)) xs)]]])

(defn debug-page [title]
  (prerender-block (find-node title)))

(defn render-reference-list [node]
  (let [them (->> (:block/_refs node)
              (group-by #(-> % :block/page :node/title))
              (sort-by first)
              (filter first))]
    (if (empty? them)
      ()
      [:section
       [:h2 "References"]
       [:ul
        (for [[reftitle refs] them]
          (render-refs reftitle refs))]])))

(defn render-page [node]
  [:article
   [:section
    [:h1 (render-inline (:node/title node))]
    (map #(render-block % 0) (node :block/children))]
   (if (empty? (node :block/_refs))
     '()
     (render-reference-list node))])

(defn single-page [node]
  (let [title (:node/title node)]
    (println "Rendering" (:block/uid node) title)
    (html5
     [:meta {:charset "utf-8"}]
     [:link {:rel "stylesheet" :href "/index.css"}]
     [:title "@meekaale " title]
     [:header
      [:a {:href "/" } "@meekaale"]
      " » "
      (match (:type (page-type title))
        :daily-note [:a {:href "/daily-notes/"} "Daily Notes"]
        :book [:a {:href "/books/"} "Books"]
        :article [:a {:href "/articles/"} "Articles"]
        :topic [:a {:href "/topics/"} "Topic"])
      " » "
      [:strong (render-inline title)]]
     (render-page node))))

(defn test-1 []
  (pprint (single-page (find-block "f4a5OO7AE"))))

(defn test-2 []
  (pprint (single-page (find-node "March 10th, 2021"))))

(def all-pages
  (into {}
        (for [node everything]
          [(str "/" (:href (page-type (:node/title node))) "/")
           (fn [context]
             (single-page node))])))

(defn pages []
  (merge
   all-pages
   (stasis/slurp-directory "resources/static/" #".")
   {"/articles/"
    (html5
     [:meta {:charset "utf-8"}]
     [:link {:rel "stylesheet" :href "/index.css"}]
     [:title "@meekaale" " » " "Articles"]
     [:header
      [:a {:href "/" } "@meekaale"]
      " » " "Articles"]
     [:article
      [:ul
       (for [x all-articles]
         (let [title (x :node/title)]
           [:li
            [:div
             [:a {:href (str "/" (slug title) "/")} (render-inline title true)]]
            [:div
             (let [author (page-author-markup x)]
               (render-inline author))]]))]])}
   {"/books/"
    (html5
     [:meta {:charset "utf-8"}]
     [:link {:rel "stylesheet" :href "/index.css"}]
     [:title "@meekaale" " » " "Books"]
     [:header
      [:a {:href "/" } "@meekaale"]
      " » " "Books"]
     [:article
      [:ul
       (for [x all-books]
         (let [title (x :node/title)]
           [:li
            [:div
             [:a {:href (str "/" (slug title) "/")} (render-inline title true)]]
            [:div
             (let [author (page-author-markup x)]
               (render-inline author))]]))]])}
   {"/daily-notes/"
    (html5
     [:meta {:charset "utf-8"}]
     [:link {:rel "stylesheet" :href "/index.css"}]
     [:title "@meekaale" " » " "Daily Notes"]
     [:header
      [:a {:href "/" } "@meekaale"]
      " » "
      [:a {:href "/daily-notes/"} "Daily Notes"]]
     [:article {:style "display: none"}
      [:table.daily-notes
       (for [[uid date title] daily-notes]
         [:tr
          [:td date]
          [:td [:a {:href (str "/" (slug date) "/")} (render-inline title true)]]]
         )
       ]
      ]
     )
    }
   {"/index.html"
    (html5
     [:meta {:charset "utf-8"}]
     [:link {:rel "stylesheet" :href "index.css"}]
     [:title "@meekaale"]
     [:header
      "@meekaale's web site"]
     (for [[uid date title] daily-notes]
       (render-page (find-node date)))
     )}))

(def app (stasis/serve-pages pages))

(defn -main [& args]
  (stasis/empty-directory! "../meekaale")
  (stasis/export-pages (pages) "../meekaale"))
