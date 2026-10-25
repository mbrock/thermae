(ns thermae.markdown
  "Markdown from the same Hiccup content used by the public HTML pages."
  (:require [clojure.string :as s]))

(declare render)

(defn- escape-text [text]
  (s/replace text #"[\\`*\[\]<>_]" #(str "\\" %)))

(defn- destination [url]
  (s/escape (str url) {\space "%20" \( "%28" \) "%29" \< "%3C" \> "%3E"}))

(defn- element [node]
  (let [[tag & body] node
        attrs (if (map? (first body)) (first body) {})
        children (if (map? (first body)) (rest body) body)
        tag (first (s/split (name tag) #"[.#]"))
        content (apply str (map render children))
        trimmed (s/trim content)
        anchor (when-let [id (:id attrs)]
                 (str "<a id=\"" (s/escape id {\& "&amp;" \" "&quot;" \< "&lt;" \> "&gt;"}) "\"></a>\n"))]
    (str anchor
         (cond
           (re-matches #"h[1-6]" tag)
           (str "\n\n" (apply str (repeat (Integer/parseInt (subs tag 1)) "#")) " " trimmed "\n\n")
           (= tag "a") (str "[" trimmed "](" (destination (:href attrs)) ")")
           (= tag "img") (str "![" (escape-text (or (:alt attrs) "")) "](" (destination (:src attrs)) ")")
           (= tag "iframe") (str "[Embedded media](" (destination (:src attrs)) ")")
           (= tag "strong") (str "**" content "**")
           (= tag "em") (str "*" content "*")
           (= tag "blockquote") (str "\n\n" (s/join "\n" (map #(str "> " %) (s/split-lines trimmed))) "\n\n")
           (= tag "li") (str "\n- " (s/replace trimmed "\n" "\n  ") "\n")
           (= tag "br") "\n"
           (#{"p" "div" "section" "article" "ul" "ol" "header"} tag) (str "\n\n" trimmed "\n\n")
           :else content))))

(defn render [node]
  (cond
    (nil? node) ""
    (string? node) (escape-text node)
    (and (vector? node) (keyword? (first node))) (element node)
    (sequential? node) (apply str (map render node))
    :else (escape-text (str node))))

(defn document [node]
  (str (-> (render node)
           (s/replace #"[ \t]+\n" "\n")
           (s/replace #"\n{3,}" "\n\n")
           s/trim)
       "\n"))
