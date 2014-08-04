(ns tfidf.core
  (:require [clojure.string :as s])
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
(defn parse-grant-file [file]
  (let [abs (last (s/split (slurp file) #"\n[^:]*:"))
        abs (s/replace abs #"[^a-zA-Z \n]" "")
        words (map s/lower-case (filter #(< 1 (count %)) (s/split abs #"[ \n]+")))
        tf (frequencies words)
        df (keys tf)]
    tf))

(defn read-data
  ([] (read-data "/home/sunil/clojure-projects/scibler/grant-subset"))
  ([directory]
     (let [files (take 20 (drop 1 (file-seq (clojure.java.io/file directory))))
           num-docs (float (count files))
           fnames (map #(.getName %) files)
           doc-tfs (map parse-grant-file files)
           dfs (apply merge-with + (map (fn [doc-tf] (zipmap (keys doc-tf) (repeat 1))) doc-tfs))
           idfs (into {} (map (fn [[term doc-term-count]] [term (Math/log (/ num-docs doc-term-count))]) dfs))
           doc-tfidfs (into {} (map (fn [file-name doc-tf]
                                      [file-name (into {} (map (fn [[term freq]]
                                                                 [term (* freq (idfs term))])
                                                               doc-tf))])
                                    fnames doc-tfs))
           inverted-index (into {}
                                (map (fn [[term posting-list]]
                                       [term (map rest posting-list)])
                                     (group-by first (for [[doc tfidfs] doc-tfidfs
                                                           [term tfidf] tfidfs]
                                                       [term doc tfidf]))))]
       inverted-index)))
(defn query [s index]
  (let [qterms (map (comp s/lower-case s/trim) (s/split (s/replace s #"[^a-zA-Z \n]" "") #"[ \n]+"))
        scored-docs (map (fn [[doc-name score-seq]]
                           [doc-name (apply + (map second score-seq))])
                         (group-by first (mapcat second (select-keys index qterms))))]
    scored-docs))
