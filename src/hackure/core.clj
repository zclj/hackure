(ns hackure.core
  (:require [hackure.assembler :as assembler])
  (:gen-class))

(defn -main
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (let [assembly-from-file (assembler/read-assembly-from-file (first args))
        hack-file (clojure.string/replace (first args) ".asm" ".hack")]
    (assembler/write-machine-code-to-file
     hack-file
     (assembler/parse assembly-from-file assembler/with-predefined-symbols))))
