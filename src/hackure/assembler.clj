(ns hackure.assembler
  (:require [hackure.utils :as utils]))

(def destinations
  {:null "000" :M "001" :D "010" :MD "011"
   :A "100" :AM "101" :AD "110" :AMD "111"})

(def jumps
  {:null "000" :JGT "001" :JEQ "010" :JGE "011"
   :JLT "100" :JNE "101" :JLE "110" :JMP "111"})

(def computations-a
  {:0 "101010" :1 "111111" :-1 "111010" :D "001100" :A "110000" :!D "001101" :!A "110001"
   :-D "001111" :-A "110011" :D+1 "011111" :A+1 "110111" :D-1 "001110" :A-1 "110010"
   :D+A "000010" :D-A "010011" :A-D "000111" :D&A "000000" :D|A "010101"})

(def computations-m
  {:M "110000" :!M "110001" :-M "110011" :M+1 "110111" :M-1 "110010" :D+M "000010"
   :D-M "010011" :M-D "000111" :D&M "000000" :D|M "010101"})

(def with-predefined-symbols
  {"SP" 0 "LCL" 1 "ARG" 2 "THIS" 3 "THAT" 4
   "R0" 0 "R1" 1 "R2" 2 "R3" 3 "R4" 4 "R5" 5 "R6" 6 "R7" 7 "R8" 8 "R9" 9
   "R10" 10 "R11" 11 "R12" 12 "R13" 13 "R14" 14 "R15" 15
   "SCREEN" 16384 "KBD" 24576})

(defn build-assign-code [a-bit cmd-c dest-c]
  (str "111" a-bit cmd-c dest-c "000"))

(defn build-jump-code [cmd-c jmp-c]
  (str "111" "0" cmd-c "000" jmp-c))

(defn parse-a-cmd [a-cmd]
  (let [address (Integer/toString (Integer. (subs a-cmd 1)) 2)
        adr-length (count address)]
    (str (apply str (drop adr-length "0000000000000000")) address)))

(defn parse-assign-c-cmd [c-cmd]
  (let [[dest cmd] (clojure.string/split c-cmd #"=")
        cmd-k (keyword cmd)
        cmd-c (cmd-k computations-a (cmd-k computations-m))
        dest-c ((keyword dest) destinations)
        a-bit (if (re-find #"M" cmd) "1" "0")]
    (build-assign-code a-bit cmd-c dest-c)))

(defn parse-jump-c-cmd [c-cmd]
  (let [[cmd jmp] (clojure.string/split c-cmd #";")
        jmp-c ((keyword jmp) jumps)
        cmd-c ((keyword cmd) computations-a)]
    (if cmd-c
      (build-jump-code cmd-c jmp-c)
      (build-jump-code ((keyword cmd) computations-m) jmp-c))))

(defn build-code-from-assembly [asm-line]
  (cond
   (re-find #"@" asm-line) (parse-a-cmd asm-line)
   (re-find #"[ADM]=-*!*[A-Z0-9]" asm-line) (parse-assign-c-cmd asm-line)
   (re-find #"[0-9ADM];[A-Z]" asm-line) (parse-jump-c-cmd asm-line)))

(defn get-label [asm-line]
  (if (re-find #"\(.+\)" asm-line)
    (clojure.string/replace asm-line #"\(*\)*" "")))

(defn get-variable [asm-line]
  (if (re-find #"@[A-Za-z]" asm-line)
    (clojure.string/replace asm-line #"@" "")))

(defn get-symbol [sym-tbl asm-line]
  (if-let [var-sym (get-variable asm-line)]
    (get sym-tbl var-sym)))

(defn remove-labels-from [assembly]
  (filter #(not (get-label %)) assembly))

(defn build-label-symbols-from [assembly]
  (let [[label-symbols _]
        (reduce (fn [[symbols rom-address] assembly-line]
                  (if-let [label (get-label assembly-line)]
                    [(assoc symbols label rom-address) rom-address]
                    [symbols (inc rom-address)]))
                [{} 0] assembly)]
    label-symbols))

(defn build-variable-symbols-from [symbol-table assembly base-adr]
  (let [symbols-in-assembly (distinct (filter #(re-find #"@[A-Za-z]" %) assembly))
        symbols (map #(clojure.string/replace % "@" "") symbols-in-assembly)
        new-symbol-entries (zipmap symbols (range base-adr (+ base-adr (count symbols))))]
    (into symbol-table new-symbol-entries)))

(defn replace-symbol-references [symbol-table assembly]
  (reduce (fn [replaced-lines next-line]
            (conj replaced-lines
                  (if-let [symbol-ref (get-symbol symbol-table next-line)]
                    (str "@" symbol-ref)
                    next-line)))
          [] (map str assembly)))

(defn build-assembly-code-with-symbols [symbol-table assembly-line]
  (if-let [variable (get-variable assembly-line)]
    (build-code-from-assembly (str "@" (get symbol-table variable)))
    (build-code-from-assembly assembly-line)))

(defn build-machine-code-from [symbol-table assembly]
  (map #(build-assembly-code-with-symbols symbol-table %) assembly))

(defn parse [dirty-assembly default-symbols]
  (let [clean-assembly (utils/clean-up-assembly dirty-assembly)
        symbols-with-labels (merge default-symbols (build-label-symbols-from clean-assembly))
        assembly-with-labels-excluded (remove-labels-from clean-assembly)
        assembly (replace-symbol-references symbols-with-labels assembly-with-labels-excluded)
        complete-symbol-table (build-variable-symbols-from symbols-with-labels assembly 16)]
    (build-machine-code-from complete-symbol-table assembly)))