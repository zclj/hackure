(ns hackure.vm-translator
  (:require [hackure.utils :as utils]))

(def registers-map
  {"local" "@LCL"
   "argument" "@ARG"
   "this" "@THIS"
   "that" "@THAT"
   "temp" "@5"
   "pointer" "@3"})

(def write-add ["@SP" "M=M-1" "A=M" "D=M" "A=A-1" "M=D+M"])

(def write-sub ["@SP" "M=M-1" "A=M" "D=M" "A=A-1" "M=M-D"])

(def write-and ["@SP" "M=M-1" "A=M" "D=M" "A=A-1" "M=M&D"])

(def write-or ["@SP" "M=M-1" "A=M" "D=M" "A=A-1" "M=M|D"])

(def write-neg ["@SP" "A=M-1" "M=-M"])

(defn write-eq [label-id]
  ["@SP" "AM=M-1" "D=M" "A=A-1" "D=D-M" "M=D-1" (str "@END_EQ_" label-id)
   "D;JEQ" "@SP" "A=M-1" "M=0" (str "(END_EQ_" label-id ")")])

(defn write-lt [label-id]
  ["@SP" "AM=M-1" "D=M" "A=A-1" "D=D-M" "M=-1" (str "@END_LT_" label-id)
   "D;JGT" "@SP" "A=M-1" "M=0" (str "(END_LT_" label-id ")")])

(defn write-gt [label-id]
  ["@SP" "AM=M-1" "D=M" "A=A-1" "D=D-M" "M=-1" (str "@END_GT_" label-id)
   "D;JLT" "@SP" "A=M-1" "M=0" (str "(END_GT_" label-id ")")])

(defn get-type [code-line]
  (condp contains? (first (clojure.string/split code-line #" "))
    #{"push"} :C_PUSH
    #{"pop"} :C_POP
    #{"gt" "lt" "eq" "neg" "or" "add" "sub" "and"} :C_ARITHMETIC))

(defn get-storage-type [code-line]
  (condp contains? code-line
    #{"local" "argument" "this" "that"} :RAM
    #{"temp" "pointer"} :REG
    #{"constant"} :CONST
    #{"static"} :STATIC
    :UNKOWN))

(defn get-arg [index code-line]
  (nth (drop 1 (clojure.string/split code-line #" ")) index nil))

(defn build-code-line-meta [code-line file]
  (let [type (get-type code-line)
        arg1 (get-arg 0 code-line)
        arg2 (get-arg 1 code-line)
        storage-type (get-storage-type arg1)]
    {:code code-line :type type :arg1 arg1 :arg2 arg2
     :file file :storage-type storage-type}))

(defn build-meta [code file]
  (reduce #(conj %1 (build-code-line-meta %2 file)) [] code))

(defmulti write-code-part
  (fn [{:keys [code type arg1 arg2 file storage-type]}]
    [type storage-type]))

(defmethod write-code-part [:C_PUSH :RAM] [{:keys [arg1 arg2]}]
  (let [address (registers-map arg1)
        index (str "@" arg2)]
    (condp = arg2
      "0" [address "A=M" "D=M" "@SP" "M=M+1" "A=M-1" "M=D"]
      "1" [address "A=M+1" "D=M" "@SP" "M=M+1" "A=M-1" "M=D"]
      [index "D=A" address "A=D+M" "D=M" "@SP" "M=M+1" "A=M-1" "M=D"])))

(defmethod write-code-part [:C_PUSH :REG] [{:keys [arg1 arg2]}]
  (let [address (registers-map arg1)
        index (str "@" arg2)]
    (condp = arg2
      "0" [address "D=M" "@SP" "M=M+1" "A=M-1" "M=D"]
      "1" [address "A=A+1" "D=M" "@SP" "M=M+1" "A=M-1" "M=D"]
      [index "D=A" address "A=D+A" "D=M" "@SP" "M=M+1" "A=M-1" "M=D"])))

(defmethod write-code-part [:C_PUSH :CONST] [{:keys [arg2]}]
  [(str "@" arg2) "D=A" "@SP" "M=M+1" "A=M-1" "M=D"])

(defmethod write-code-part [:C_PUSH :STATIC] [{:keys [file arg2]}]
  (let [address (str "@" file "." arg2)]
    [address "D=M" "@SP" "M=M+1" "A=M-1" "M=D"]))

(defmethod write-code-part [:C_POP :RAM] [{:keys [arg1 arg2]}]
  (let [address (registers-map arg1)
        index (str "@" arg2)]
    (condp = arg2
      "0" ["@SP" "AM=M-1" "D=M" address "A=M" "M=D"]
      "1" ["@SP" "AM=M-1" "D=M" address "A=M+1" "M=D"]
      [index "D=A" address "D=D+M" "@R13" "M=D"
       "@SP" "AM=M-1" "D=M" "@R13" "A=M" "M=D"])))

(defmethod write-code-part [:C_POP :REG] [{:keys [arg1 arg2]}]
  (let [address (registers-map arg1)
        index (str "@" arg2)]
    (condp = arg2
      "0" ["@SP" "AM=M-1" "D=M" address "M=D"]
      "1" ["@SP" "AM=M-1" "D=M" address "A=A+1" "M=D"]
      [index "D=A" address "D=D+A" "@R13" "M=D"
       "@SP" "AM=M-1" "D=M" "@R13" "A=M" "M=D"])))

(defmethod write-code-part [:C_POP :STATIC] [code-meta]
  (let [address (str "@" (:file code-meta) "." (:arg2 code-meta))]
    ["@SP" "AM=M-1" "D=M" address "M=D"]))

(defmethod write-code-part [:C_ARITHMETIC :UNKOWN] [code-meta]
  (condp = (:code code-meta)
    "add" write-add
    "sub" write-sub
    "and" write-and
    "or" write-or
    "neg" write-neg
    "eq" (write-eq (java.util.UUID/randomUUID))
    "lt" (write-lt (java.util.UUID/randomUUID))
    "gt" (write-gt (java.util.UUID/randomUUID))))

(defn write-code [code-metas]
  (reduce #(into %1 (write-code-part %2)) [] code-metas))

(defn parse
  ([code] (parse code "no-source-file"))
  ([code file-name]
     (-> code
         (utils/clean-up-vm-code)
         (build-meta file-name)
         (write-code))))

(defn parse-source [file-source]
  (let [[source file-ext] (clojure.string/split file-source #"\.")]
    (if file-ext
      [(str source ".asm") (parse (utils/read-code-from-file file-source) source)]
      (let [source-dir (clojure.string/replace source "/" "")
            files (filter #(re-find #"\.vm" %) (utils/get-file-names-from-dir source-dir))]
        [(str source-dir ".asm")
         (flatten (map #(parse (utils/read-code-from-file (str source-dir "/" %))) files))]))))