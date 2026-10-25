(ns thermae.markdown-test
  (:require [clojure.test :refer :all]
            [thermae.markdown :as md]))

(deftest published-structure-survives
  (is (= "# Title\n\nA **bold** and *quiet* [link](/somewhere#quote).\n\n> Words\n> on two lines\n"
         (md/document
          [:article.page [:h1 "Title"]
           [:p "A " [:strong "bold"] " and " [:em "quiet"] " "
            [:a.ref {:href "/somewhere#quote"} "link"] "."]
           [:blockquote "Words\non two lines"]]))))

(deftest anchors-media-and-nested-lists
  (let [text (md/document
              [:section
               [:p {:id "quote-123"} "A quote"]
               [:ul [:li "Parent" [:ul [:li "Child"]]]]
               [:img {:src "https://example.org/a (b).png" :alt "An image"}]
               [:iframe {:src "https://www.youtube.com/embed/123"}]])]
    (is (.contains text "<a id=\"quote-123\"></a>"))
    (is (.contains text "- Parent\n\n  - Child"))
    (is (.contains text "![An image](https://example.org/a%20%28b%29.png)"))
    (is (.contains text "[Embedded media](https://www.youtube.com/embed/123)"))))

(deftest text-is-not-markdown-instructions
  (is (= "\\[literal\\] \\*stars\\* \\<tag\\> \\_value\\_\n"
         (md/document [:p "[literal] *stars* <tag> _value_"]))))
