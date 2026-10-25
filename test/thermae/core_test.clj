(ns thermae.core-test
  (:require [clojure.test :refer :all]
            [thermae.core :refer :all]))

(deftest markdown-exports-cover-only-published-pages
  (let [exports (pages)]
    (is (= (inc (count all-pages))
           (count (filter #(.endsWith % "/index.md") (keys exports)))))
    (is (not (contains? exports "/mikaelogy.edn")))
    (is (.contains (get exports "/robots.txt") "User-agent: *"))
    (is (.contains ((get exports "/index.md") {}) "Complete page directory"))))

(deftest markdown-page-retains-links-and-published-filtering
  (with-redefs [pages-by-title {"Example" {:node/title "Example"}}]
    (let [text (markdown-page {:node/title "Example"
                               :block/children [{:block/string "Text **bold** [[Example]]"}
                                                {:block/string "**Tags** hidden-tag-row"}
                                                {:block/string "> A quotation"}]})]
      (is (.contains text "# Example"))
      (is (.contains text "**bold**"))
      (is (.contains text "[Example]("))
      (is (.contains text "> A quotation"))
      (is (not (.contains text "hidden-tag-row"))))))
