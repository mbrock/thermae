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

(def website-name "goula.sh")

(defn slurp-resource [path] (->> path io/resource slurp))

(defn read-datascript-edn [string]
  (edn/read-string {:readers d/data-readers} string))

(def raw-db
  (read-datascript-edn (slurp-resource "mikaelogy.edn")))

(def markup-parser
  (insta/parser (slurp-resource "markup-grammar.txt")))

(def page-pull
  '[:node/title
    :create/time
    :block/uid
    :block/string
    :block/order
    :block/heading
    {:block/page [:node/title]}
    {:block/refs [:block/uid :node/title]}
    {:block/_refs
     [:block/uid :block/string
      {:block/page [:node/title]}]}
    {:block/children ...}])

(def ancestor-rule
  '[[(ancestor ?b ?a)
     [?a :block/children ?b]]
    [(ancestor ?b ?a)
     [?parent :block/children ?b]
     (ancestor ?parent ?a)]])

(defn pageless-blocks [db]
  (d/q
   '[:find ?x ?page
     :in $ %
     :where
     [?x :block/uid ?uid]
     (not [?x :node/title])
     (not [?x :block/page])
     [_ :block/children ?x]
     (ancestor ?x ?page)
     [?page :node/title ?title]]
   db
   ancestor-rule))

(def fixed-db
  (d/db-with raw-db
             (vec
              (for [[block page] (pageless-blocks raw-db)]
                [:db/add block :block/page page]))))

(defn query [q & xs]
  (apply d/q (concat (list q fixed-db) xs)))

(def pages-by-title
  (let [pages (query
               '[:find (pull ?e pattern)
                 :in $ pattern
                 :where [?e :node/title]]
               page-pull)]
    (into {}
          (for [[x] pages]
            [(:node/title x) x]))))

(defn page-attribute [attr page]
  (let [prefix (str attr ":: ")
        string (->> page :block/children
                    (map :block/string)
                    (filter #(s/starts-with? % prefix))
                    first)]
    (if string
      (s/replace string prefix ""))))

(defn day-uid? [uid]
  (boolean (re-matches #"(..)-(..)-(....)" uid)))

(def uid-day-formatter
  (java.time.format.DateTimeFormatter/ofPattern "MM-dd-yyyy"))

(def pretty-day-formatter
  (java.time.format.DateTimeFormatter/ofPattern "E, d MMM yyyy"))

(defn parse-day-uid [uid]
  (java.time.LocalDate/parse uid uid-day-formatter))

(defn pretty-date [date]
  (.format date pretty-day-formatter))

(defn recent-quotes-query []
  (query
   '[:find (pull ?x pattern)
     :in $ pattern
     :where
     [?x :block/string ?s]
     (or
      [(clojure.string/starts-with? ?s "Highlights first synced")]
      [(clojure.string/starts-with? ?s "New highlights added")])]
   page-pull))

(defn days-referenced-by-block [block]
  (->> block
       :block/refs
       (map :block/uid)
       (filter day-uid?)
       (map parse-day-uid)))

(defn block-page-title [block]
  (-> block :block/page :node/title))

(def recent-quotes
  (->> (recent-quotes-query)
       (map first)
       (filter #(not (s/starts-with? (block-page-title %)
                                     "[[podcasts]]")))
       (sort-by #(first (days-referenced-by-block %)))
       (reverse)
       (take 10)))

(defn change-date-format [date]
  (match (re-matches #"(..)-(..)-(....)" date)
    [_ m d y] (s/join "-" [y m d])
    _ date))

(defn page-type [title]
  (if (re-matches #"[A-Z].*? [0-9]+(th|nd|st|rd), [0-9]{4}" title)
    {:type :daily-note :href (str "days/"
                                  (change-date-format
                                   (:block/uid (pages-by-title title))))}
    (match (re-matches #"\[\[(.*?)\]\] (.*)" title)
      [_ "books" x]    {:type :book :href (str "books/" (->slug x))}
      [_ "articles" x] {:type :article :href (str "articles/" (->slug x))}
      _ {:type :topic :href (str "topics/" (->slug title))}
      )))

(defn process-body [x]
  (-> x
      (s/replace #"(\D)\.[0-9]+\b" "$1.")
      (s/replace #"\{\{word-count\}\}" "")
      (s/replace #" \(\[Location.*?\]\(.*?\)\)" "")
      (s/replace #"Highlights first synced by #Readwise (.*)" "Highlights from $1.")
      (s/replace #"New highlights added (.*?) at .*" "Highlights from $1.")))

(defn slug [title]
  (:href (page-type title)))

(def daily-notes
  (reverse
   (sort-by
       #(.parse (java.text.SimpleDateFormat. "MM-dd-yyy")
                (first %))
     (query
      '[
        :find ?uid ?date ?title ?summary
        :where
        [?page :block/uid ?uid]
        [?x :block/page ?page]
        [?page :node/title ?date]
        [?x :block/refs ?attr]
        [?x :block/refs ?value]
        [?x :block/string ?title]
        [?attr :node/title "Note"]
        [?summary-block :block/page ?page]
        [?summary-block :block/refs ?summary-ref]
        [?summary-ref :node/title ".Summary"]
        [?summary-block :block/string ?summary]
        ]
      )))
  )

(defn title->uid [title]
  (:block/uid (pages-by-title title)))

(def interesting-page-title?
  (complement #{".Summary" "articles" "books" "video" "Note"}))

(defn find-block [uid]
  (let [it
        (query '[
                 :find (pull ?e [:block/string
                                 :db/id
                                 :block/page
                                 {:block/page [:node/title]}
                                 :block/children {:block/children ...}])
                 :in $ ?uid
                 :where [?e :block/uid ?uid]]
               uid)]
    (first (first it))))

(defn transform-tree-to-link [tree]
  (let [go transform-tree-to-link]
    (match tree
      [:inline & xs] (apply str (map go xs))
      [:word x] x
      [:symbol x] x
      [:link x] (str "[[" (go x) "]]")
      _ (throw (ex-info "transform-tree-to-link" {:tree tree})))))

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

      [:paragraph [:inline [:blockref [:uid x]]]]
      (let [it (find-block x)
            title (-> it :block/page :node/title)
            content (:block/string it)
            name (go-without-links (markup-parser content))]
        (if no-links? name
            [:blockquote.blockref
             [:a {:title title :href (str "/" (slug title) "#" x)} name]]))

      [:blockref [:uid x]]
      (let [it (find-block x)
            title (-> it :block/page :node/title)
            content (:block/string it)
            name (go-without-links (markup-parser content))]
        (if no-links? name
            [:a {:class "blockref" :title title :href (str "/" (slug title) "#" x)} name]))

      [:attr [:word "Note"] y]
      (list "📝 " (go y))

      [:attr x y]
      (list [:strong (go x)] ": " (go y))

      [:classtag _] nil

      [:text x] [:span (go x)]
      [:word x] x
      [:italic x] [:em (go x)]
      [:bold x] [:strong (go x)]

      [:paragraph x] (go x)

      [:url url]
      (if no-links? url
          [:a {:href url :target "_blank"} url])

      [:inline & xs] (map go xs)

      [:link [:inline [:word "books"]]]
      "📙"

      [:link [:inline [:word "articles"]]]
      "📰"

      [:link x]
      (if no-links? (go x)
          [:a.ref {:href (link-href x)}
           (go-without-links x)])

      [:img x] [:img {:src x}]

      [:parenthesis [:inline [:xlink [:word "View Highlight"] _]]]
      nil

      [:parenthesis [:inline & xs]] (go `[:inline [:symbol "("] ~@xs [:symbol ")"]])

      [:blockquote x] [:blockquote (go x)]
      [:symbol x] x

      [:alias [:symbol "*"] [:uid x]]
      (let [it (find-block x)
            title (-> it :block/page :node/title)
            content (:block/string it)
            name (go-without-links (markup-parser content))]
        (if no-links? name
            [:a {:class "cite" :title "Go to quote" :href (str "/" (slug title) "#" x)} "*"]))

      [:xlink text url]
      (if no-links? (go text)
          [:a.xlink {:href url} (go-without-links text)])

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
  ([body] (render-body body 0 nil nil))
  ([body indent heading] (render-body body indent heading () nil))
  ([body indent heading tags uid]
   (let [tree (markup-parser (process-body body))]
     (if (insta/failure? tree)
       (throw (ex-info "parse error" tree))
       (let [html (tree-to-html tree)
             attrs (merge
                    (if uid {:id uid} {})
                    (if (empty? tags) {} {:class (s/join " " tags)}))]
         (match html
           [:blockquote x]
           [:blockquote attrs x]

           xs (if heading
                [(keyword (str "h" heading)) attrs html]
                [:p attrs html])))))))

(defn render-block [x indent]
  (let [body (x :block/string)
        tags (->> x
                  :block/refs
                  (map :node/title)
                  (filter identity)
                  (filter #(s/starts-with? % "."))
                  (map #(subs % 1)))
        kids (map #(render-block % (inc indent))
                  (sort-by :block/order (x :block/children)))]
    (if (s/blank? body)
      kids
      (if (s/starts-with? body "**Tags**")
        ()
        (list (render-body body indent (:block/heading x) tags (:block/uid x))
              (if (not (empty? kids))
                [:div.indent kids]))))))

(defn render-refs [title xs]
  [:li
   [:div.linkref
    [:h3 [:a {:href (str "/" (slug title))} (render-inline title true)]]
    [:div
     (map (fn [x]
            (render-body (:block/string x) 0 nil)) xs)]]])

(defn pages-mentioned-by [uid]
  ;; XXX: Horrible kludge because I don't want to do tree traversing.
  ;; I want the links in order of appearance.
  (map #(-> %
            (s/replace #"📙" "[[books]]")
            (s/replace #"📰" "[[articles]]")
            )
       (distinct (for [[url title]
                       (re-seq #"<a class=\"ref\" .*?>(.*?)</a>"
                               (html5 (render-block (find-block uid) 0)))

                       ]
                   title))))

(defn render-reference-list [node]
  (let [them (->> (:block/_refs node)
              (group-by #(-> % :block/page :node/title))
              (sort-by first)
              (filter first))]
    (if (empty? them)
      ()
      [:section.references
       [:h1.semibold.mb-1 "Incoming Links"]
       [:ul
        (for [[reftitle refs] them]
          (render-refs reftitle refs))]])))

(defn render-page [node]
  [:article.page
   [:section
    [:h1 (render-inline (:node/title node))]
    (map #(render-block % 0) (node :block/children))]
   (render-reference-list node)])

(defn twitter [tag text]
  [:meta {:name (str "twitter:" tag) :content text}])

(defn html-page [title & contents]
  (html5
   [:meta {:charset "utf-8"}]
   [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]

   [:link {:rel "stylesheet" :href "/index.css"}]

   (twitter "card" "summary")
   (twitter "site" "@meekaale")
   (twitter "image" "https://goula.sh/logo-square.png")

   contents))

(defn daily-note-info [node]
  (if-let [note (first (filter #(s/starts-with? (:block/string %) "Note:: ")
                         (:block/children node)))]
    (let [first-block (first (sort-by :block/order (:block/children note)))]
      {:title (s/replace (:block/string note) "Note:: " "")
       :summary (s/replace (:block/string first-block) "#.Summary " "")})))

(defn breadcrumb [& xs]
  (interpose " • " (filter identity xs)))

(defn emojify-title [title]
  (-> title
      (s/replace "[[books]]" "📙")
      (s/replace "[[articles]]" "📰")))

(defn single-page [node]
  (let [title (:node/title node)]
    (println "Rendering" (:block/uid node) title)
    (html-page (emojify-title title)
               (match (:type (page-type title))
                 :daily-note
                 (if-let [info (daily-note-info node)]
                   (list
                    (twitter "title" (emojify-title (:title info)))
                    (twitter "description" (emojify-title (:summary info))))
                   (list
                    (twitter "title" (emojify-title title))))
                 :book
                 (list
                  (twitter "title" (emojify-title title))
                  (twitter "description" (str "Highlights from " (emojify-title title))))
                 :article
                 (list
                  (twitter "title" title)
                  (twitter "description" (str "Highlights from " (emojify-title title))))
                 _ ())

               [:title (str (emojify-title title) " - " website-name)]
               [:header
                (breadcrumb [:a {:href "/" } website-name])]
               (render-page node))))

(def all-pages
  (into {}
        (for [[title node] pages-by-title]
          [(str "/" (:href (page-type title)) "/")
           (fn [context]
             (single-page node))])))

(defn adequate-quote? [block]
  (> (count (re-seq #" " (:block/string block))) 10))

(defn index-page [ctx]
  (html-page
   "@meekaale"
   (list
    (twitter "title" "goula.sh")
    (twitter "description" "Notes and ramblings by @meekaale."))
   [:title website-name]
   [:header
    (breadcrumb
     [:span website-name]
     "Welcome")]

   [:div.page
    [:section.article-list
     [:h1.mb-1.semibold "Daily Notes"]
     (for [[uid date title summary] daily-notes]
       [:article
        [:div.small.date date]
        [:h2
         [:a.semibold {:href (str "/" (slug date) "/")}
          (render-inline title true)]]
        [:div (render-inline summary)]
        (let [mentions (pages-mentioned-by uid)]
          (if (not (empty? mentions))
            [:div.mentions-list.small.mt-1.flex.gap-r1
             [:span "🔗"]
             [:ul.flex.wrap.gap-r1
              (for [topic mentions]
                (do
                  [:li (render-inline (str "[[" topic "]]"))]))]]))
        ]
       )]
    [:section.article-list
     [:h1.semibold.mb-1 "Recent Highlights"]
     (for [block recent-quotes]
       (let [date (first (days-referenced-by-block block))
             title (-> block :block/page :node/title)
             children (-> block :block/children)]
         [:article
          [:div.small.date (pretty-date date)]
          [:h2
           [:a.semibold {:href (str "/" (slug title) "/")}
            (render-inline title true)]]
          [:ul
           (for [x (take 3 (filter adequate-quote? children))]
             (render-body (:block/string x) 0 nil nil nil))]]))]]))

(defn pages []
  (merge
   all-pages
   (stasis/slurp-directory "resources/static/" #"\.css$")
   {"/index.html" index-page}))

(def app (stasis/serve-pages pages))

(defn -main [& args]
  ;(stasis/empty-directory! "../meekaale")
  (stasis/export-pages (pages) "../meekaale"))
