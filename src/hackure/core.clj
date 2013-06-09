(ns hackure.core
  (:require [hackure.assembler :as assembler]
            [hackure.vm-translator :as vm]
            [hackure.utils :as utils])
  (:gen-class))

(defn -main
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (if-let [asm-file (re-find #".*\.asm" (first args))]
    (let [assembly-from-file (utils/read-code-from-file asm-file)
          hack-file (clojure.string/replace asm-file ".asm" ".hack")]
      (utils/write-code-to-file
       hack-file
       (assembler/parse assembly-from-file assembler/with-predefined-symbols)))
    (let [file-source (first args)
          [code-file code] (vm/parse-source file-source)]
      (do
        (println (str "Written code to : " code-file))
        (utils/write-code-to-file code-file code)))))
