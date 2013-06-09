(ns hackure.utils
  (:require [clojure.java.io :as io]))

(defn read-code-from-file [file-name]
  (with-open [rdr (io/reader file-name)]
    (doall (line-seq rdr))))

(defn write-code-to-file [file-name code]
  (with-open [writer (io/writer file-name)]
    (.write writer (clojure.string/join "\n" code))))

(defn comment? [line]
  (not (or (= "" line) (= '(\/ \/) (take 2 line)))))

(defn get-file-names-from-dir [dir]
  (->> dir
       (clojure.java.io/file)
       (file-seq)
       (filter #(.isFile %))
       (map #(.getName %))))

(defn clean-up-vm-code [code]
  (->> (filter comment? code)
       (map #(first (clojure.string/split % #"//")))
       (map clojure.string/trim)))

(defn clean-up-assembly [assembly]
  (->> (filter comment? assembly)
       (map clojure.string/trim)
       (map #(first (clojure.string/split % #" ")))))