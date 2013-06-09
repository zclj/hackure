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

(def write-not ["@SP" "A=M-1" "M=!M"])

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
    #{"gt" "lt" "eq" "neg" "or" "add" "sub" "and" "not"} :C_ARITHMETIC
    #{"label"} :C_LABEL
    #{"if-goto"} :C_IF
    #{"goto"} :C_GOTO
    #{"function"} :C_FUNCTION
    #{"return"} :C_RETURN
    #{"call"} :C_CALL))

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
    "not" write-not
    "neg" write-neg
    "eq" (write-eq (java.util.UUID/randomUUID))
    "lt" (write-lt (java.util.UUID/randomUUID))
    "gt" (write-gt (java.util.UUID/randomUUID))))

(defmethod write-code-part [:C_LABEL :UNKOWN] [{:keys [arg1]}]
  [(str "(" arg1 ")")])

(defmethod write-code-part [:C_IF :UNKOWN] [{:keys [arg1]}]
  ["@SP" "AM=M-1" "D=M" (str "@" arg1) "D;JNE"])

(defmethod write-code-part [:C_GOTO :UNKOWN] [{:keys [arg1]}]
  [(str "@" arg1) "0;JMP"])

(defmethod write-code-part [:C_FUNCTION :UNKOWN] [{:keys [arg1 arg2]}]
  (let [locals (range 0 (Integer/parseInt arg2))
        loc-code
        (for [l locals]
          [(str "@" l) "D=A" "@LCL" "A=D+M"
           "D=M" "@SP" "M=M+1" "A=M-1" "M=D"])]
    (into [(str "(" arg1 ")")] (flatten loc-code))))

(defmethod write-code-part [:C_RETURN :UNKOWN] [{:keys [arg1]}]
  ["@LCL" "D=M" "@R13" "M=D"
   "@5" "A=D-A" "D=M" "@R14" "M=D"
   "@SP" "AM=M-1" "D=M"
   "@ARG" "A=M" "M=D" "D=A" "@SP" "M=D+1" "@R13" "AM=M-1" "D=M"
   "@THAT" "M=D" "@R13" "AM=M-1" "D=M"
   "@THIS" "M=D" "@R13" "AM=M-1" "D=M"
   "@ARG" "M=D" "@R13" "AM=M-1" "D=M"
   "@LCL" "M=D"
   "@R14" "A=M" "0;JMP"])

(defmethod write-code-part [:C_CALL :UNKOWN] [{:keys [arg1 arg2]}]
  (let [return-address (str "RET_"(java.util.UUID/randomUUID))
        arguments (vec (for [x (range 0 (Integer/parseInt arg2))] "D=D-1"))]
    (vec
     (concat ["@SP" "D=M-1"]
             arguments
             ["@R13" "M=D" (str "@" return-address) "D=A" "@SP" "M=M+1" "A=M-1"
              "M=D"
              "@LCL" "D=M" "@SP" "M=M+1" "A=M-1" "M=D"
              "@ARG" "D=M" "@SP" "M=M+1" "A=M-1" "M=D"
              "@THIS" "D=M" "@SP" "M=M+1" "A=M-1" "M=D"
              "@THAT" "D=M" "@SP" "M=M+1" "A=M-1" "M=D"
              "@R13" "D=M+1" "@ARG" "M=D" "@SP" "D=M" "@LCL" "M=D"
              (str "@" arg1) "0;JMP" (str "(" return-address ")")]))))

(defn write-code [code-metas]
  (reduce #(into %1 (write-code-part %2)) [] code-metas))

;; TODO - refactor to use same code as "call" to save registers
(defn write-bootstrap []
  ["@256" "D=A" "@SP" "M=D"
   "@bootstrap_return" "D=A" "@SP" "M=M+1" "A=M-1" "M=D"
   "@LCL" "D=M" "@SP" "M=M+1" "A=M-1" "M=D"
   "@ARG" "D=M" "@SP" "M=M+1" "A=M-1" "M=D"
   "@THIS" "D=M" "@SP" "M=M+1" "A=M-1" "M=D"
   "@THAT" "D=M" "@SP" "M=M+1" "A=M-1" "M=D"
   "@Sys.init" "0;JMP" "(bootstrap_return)"])

(defn parse
  ([code] (parse code "no-source-file"))
  ([code file-name]
     (-> code
         (utils/clean-up-vm-code)
         (build-meta file-name)
         (write-code))))

(defn with-bootstrap [code]
  (into (write-bootstrap) code))

;; TODO - refactor dir/file handling
(defn parse-source [file-source]
  (let [[source file-ext] (clojure.string/split file-source #"\.vm")]
    (if file-ext
      [(str source ".asm") (with-bootstrap (parse (utils/read-code-from-file file-source) source))]
      (let [source-dir (if (= (last source) \/) (subs source 0 (- (count source) 1)) source)
            files (filter #(re-find #"\.vm" %) (utils/get-file-names-from-dir source-dir))]
        [(str source-dir ".asm")
         (with-bootstrap
           (flatten (map
                     #(parse
                       (utils/read-code-from-file (str source-dir "/" %))
                       (first (clojure.string/split % #"\."))) files)))]))))