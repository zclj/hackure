(ns hackure.vm-translator-tests
  (:use clojure.test
        hackure.vm-translator))

;;;;;;;;;;;;;;;;;;; comments ;;;;;;;;;;;;;;;;
(deftest remove-comments
  (testing "remove comments from code"
    (is (= (parse ["//comment" "push constant 8"])
           ["@8" "D=A" "@SP" "M=M+1" "A=M-1" "M=D"]))))

(deftest remove-comments-inline
  (testing "remove comments inline"
    (is (= (parse ["add                     // check if n < 2"])
           ["@SP" "M=M-1" "A=M" "D=M" "A=A-1" "M=D+M"]))))

;;;;;;;;;;;;;;;;;;; push ;;;;;;;;;;;;;;;;;;;;
(deftest push-constant
  (testing "write code for push constant 8"
    (is (= (parse ["push constant 8"])
           ["@8" "D=A" "@SP" "M=M+1" "A=M-1" "M=D"]))))

(deftest push-static
  (testing "write code for push static 8"
    (is (= (parse ["push static 8"])
           ["@no-source-file.8" "D=M" "@SP" "M=M+1" "A=M-1" "M=D"]))
        (is (= (parse ["push static 8"] "test")
           ["@test.8" "D=M" "@SP" "M=M+1" "A=M-1" "M=D"]))))

(deftest push-local
  (testing "write code for push local n"
    (is (= (parse ["push local 0"])
           ["@LCL" "A=M" "D=M" "@SP" "M=M+1" "A=M-1" "M=D"]))
    (is (= (parse ["push local 1"])
           ["@LCL" "A=M+1" "D=M" "@SP" "M=M+1" "A=M-1" "M=D"]))
    (is (= (parse ["push local 2"])
           ["@2" "D=A" "@LCL" "A=D+M" "D=M" "@SP"
            "M=M+1" "A=M-1" "M=D"]))))

(deftest push-argument
  (testing "write code for push argument n"
    (is (= (parse ["push argument 0"])
           ["@ARG" "A=M" "D=M" "@SP" "M=M+1" "A=M-1" "M=D"]))
    (is (= (parse ["push argument 1"])
           ["@ARG" "A=M+1" "D=M" "@SP" "M=M+1" "A=M-1" "M=D"]))
    (is (= (parse ["push argument 2"])
           ["@2" "D=A" "@ARG" "A=D+M" "D=M" "@SP"
            "M=M+1" "A=M-1" "M=D"]))))

(deftest push-this
  (testing "write code for push this n"
    (is (= (parse ["push this 0"])
           ["@THIS" "A=M" "D=M" "@SP" "M=M+1" "A=M-1" "M=D"]))
    (is (= (parse ["push this 1"])
           ["@THIS" "A=M+1" "D=M" "@SP" "M=M+1" "A=M-1" "M=D"]))
    (is (= (parse ["push this 2"])
           ["@2" "D=A" "@THIS" "A=D+M" "D=M" "@SP"
            "M=M+1" "A=M-1" "M=D"]))))

(deftest push-that
  (testing "write code for push that n"
    (is (= (parse ["push that 0"])
           ["@THAT" "A=M" "D=M" "@SP" "M=M+1" "A=M-1" "M=D"]))
    (is (= (parse ["push that 1"])
           ["@THAT" "A=M+1" "D=M" "@SP" "M=M+1" "A=M-1" "M=D"]))
    (is (= (parse ["push that 2"])
           ["@2" "D=A" "@THAT" "A=D+M" "D=M" "@SP"
            "M=M+1" "A=M-1" "M=D"]))))

(deftest push-temp
  (testing "write code for push temp n"
    (is (= (parse ["push temp 0"])
           ["@5" "D=M" "@SP" "M=M+1" "A=M-1" "M=D"]))
    (is (= (parse ["push temp 1"])
           ["@5" "A=A+1" "D=M" "@SP" "M=M+1" "A=M-1" "M=D"]))
    (is (= (parse ["push temp 2"])
           ["@2" "D=A" "@5" "A=D+A" "D=M" "@SP"
            "M=M+1" "A=M-1" "M=D"]))))

(deftest push-pointer
  (testing "write code for push temp n"
    (is (= (parse ["push pointer 0"])
           ["@3" "D=M" "@SP" "M=M+1" "A=M-1" "M=D"]))
    (is (= (parse ["push pointer 1"])
           ["@3" "A=A+1" "D=M" "@SP" "M=M+1" "A=M-1" "M=D"]))))

;;;;;;;;;;;;;;;;;; pop ;;;;;;;;;;;;;;;;;
(deftest pop-static
  (testing "pop into static"
    (is (= (parse ["pop static 8"])
           ["@SP" "AM=M-1" "D=M" "@no-source-file.8" "M=D"]))
    (is (= (parse ["pop static 8"] "test")
           ["@SP" "AM=M-1" "D=M" "@test.8" "M=D"]))))

(deftest pop-local
  (testing "pop into local n"
    (is (= (parse ["pop local 0"])
           ["@SP" "AM=M-1" "D=M" "@LCL" "A=M" "M=D"]))
    (is (= (parse ["pop local 1"])
           ["@SP" "AM=M-1" "D=M" "@LCL" "A=M+1" "M=D"]))
    (is (= (parse ["pop local 2"])
           ["@2" "D=A" "@LCL" "D=D+M" "@R13" "M=D"
            "@SP" "AM=M-1" "D=M" "@R13" "A=M" "M=D"]))))

(deftest pop-argument
  (testing "pop into argument n"
    (is (= (parse ["pop argument 0"])
           ["@SP" "AM=M-1" "D=M" "@ARG" "A=M" "M=D"]))
    (is (= (parse ["pop argument 1"])
           ["@SP" "AM=M-1" "D=M" "@ARG" "A=M+1" "M=D"]))
    (is (= (parse ["pop argument 2"])
           ["@2" "D=A" "@ARG" "D=D+M" "@R13" "M=D"
            "@SP" "AM=M-1" "D=M" "@R13" "A=M" "M=D"]))))

(deftest pop-this
  (testing "pop into this n"
    (is (= (parse ["pop this 0"])
           ["@SP" "AM=M-1" "D=M" "@THIS" "A=M" "M=D"]))
    (is (= (parse ["pop this 1"])
           ["@SP" "AM=M-1" "D=M" "@THIS" "A=M+1" "M=D"]))
    (is (= (parse ["pop this 2"])
           ["@2" "D=A" "@THIS" "D=D+M" "@R13" "M=D"
            "@SP" "AM=M-1" "D=M" "@R13" "A=M" "M=D"]))))

(deftest pop-that
  (testing "pop into that n"
    (is (= (parse ["pop that 0"])
           ["@SP" "AM=M-1" "D=M" "@THAT" "A=M" "M=D"]))
    (is (= (parse ["pop that 1"])
           ["@SP" "AM=M-1" "D=M" "@THAT" "A=M+1" "M=D"]))
    (is (= (parse ["pop that 2"])
           ["@2" "D=A" "@THAT" "D=D+M" "@R13" "M=D"
            "@SP" "AM=M-1" "D=M" "@R13" "A=M" "M=D"]))))

(deftest pop-temp
  (testing "pop into temp n"
    (is (= (parse ["pop temp 0"])
           ["@SP" "AM=M-1" "D=M" "@5" "M=D"]))
    (is (= (parse ["pop temp 1"])
           ["@SP" "AM=M-1" "D=M" "@5" "A=A+1" "M=D"]))
    (is (= (parse ["pop temp 2"])
           ["@2" "D=A" "@5" "D=D+A" "@R13" "M=D"
            "@SP" "AM=M-1" "D=M" "@R13" "A=M" "M=D"]))))

(deftest pop-pointer
  (testing "pop into pointer n"
    (is (= (parse ["pop pointer 0"])
           ["@SP" "AM=M-1" "D=M" "@3" "M=D"]))
    (is (= (parse ["pop pointer 1"])
           ["@SP" "AM=M-1" "D=M" "@3" "A=A+1" "M=D"]))))

;;;;;;;;;;;;;;; arithmetic operations
(deftest add
  (testing "write code for add"
    (is (= (parse ["add"])
           ["@SP" "M=M-1" "A=M" "D=M" "A=A-1" "M=D+M"]))))

(deftest sub
  (testing "write code for sub"
    (is (= (parse ["sub"])
           ["@SP" "M=M-1" "A=M" "D=M" "A=A-1" "M=M-D"]))))

(deftest and-test
  (testing "write code for and"
    (is (= (parse ["and"])
           ["@SP" "M=M-1" "A=M" "D=M" "A=A-1" "M=M&D"]))))

(deftest or-test
  (testing "write code for or"
    (is (= (parse ["or"])
           ["@SP" "M=M-1" "A=M" "D=M" "A=A-1" "M=M|D"]))))

(deftest not-test
  (testing "write code for not"
    (is (= (parse ["not"])
           ["@SP" "A=M-1" "M=!M"]))))

(deftest neg
  (testing "write code for neg"
    (is (= (parse ["neg"])
           ["@SP" "A=M-1" "M=-M"]))))

(deftest eq
  (testing "write code for eq"
    (let [asm (parse ["eq"])]
      (is (= (filter #(not (re-find #"END" %)) asm)
             ["@SP" "AM=M-1" "D=M" "A=A-1" "D=D-M" "M=D-1" "D;JEQ" "@SP" "A=M-1" "M=0"]))
      (is (= (re-find #"@END_EQ_" (asm 6))
             "@END_EQ_"))
      (is (= (re-find #"\(END_EQ_" (asm 11))
             "(END_EQ_")))))

(deftest lt
  (testing "write code for lt"
    (let [asm (parse ["lt"])]
      (is (= (filter #(not (re-find #"END" %)) asm)
             ["@SP" "AM=M-1" "D=M" "A=A-1" "D=D-M" "M=-1" "D;JGT" "@SP" "A=M-1" "M=0"]))
      (is (= (re-find #"@END_LT_" (asm 6))
             "@END_LT_"))
      (is (= (re-find #"\(END_LT_" (asm 11))
             "(END_LT_")))))

(deftest gt
  (testing "write code for gt"
    (let [asm (parse ["gt"])]
      (is (= (filter #(not (re-find #"END" %)) asm)
             ["@SP" "AM=M-1" "D=M" "A=A-1" "D=D-M" "M=-1" "D;JLT" "@SP" "A=M-1" "M=0"]))
      (is (= (re-find #"@END_GT_" (asm 6))
             "@END_GT_"))
      (is (= (re-find #"\(END_GT_" (asm 11))
             "(END_GT_")))))

;;;;;;;;;;;;;;;;;;;;;;;; Program Flow Commands ;;;;;;;;;;;;;;;;
(deftest label
  (testing "write code for label"
    (is (= (parse ["label null$LOOP_START"])
           ["(null$LOOP_START)"]))))

(deftest goto
  (testing "write code for goto"
    (is (= (parse ["goto null$LOOP_START"])
           ["@null$LOOP_START" "0;JMP"]))))

(deftest if-goto
  (testing "write code for if-goto"
    (is (= (parse ["if-goto null$LOOP_START"])
           ["@SP" "AM=M-1" "D=M" "@null$LOOP_START" "D;JNE"]))))

;;;;;;;;;;;;;;;;;;;;;;;; Function Calling ;;;;;;;;;;;;;;;;;;;;;

(deftest function
  (testing "write code for function"
    (is (= (parse ["function SimpleFunc.test 2"])
           ["(SimpleFunc.test)" "@0" "D=A" "@LCL" "A=D+M" "D=M" "@SP" 
            "M=M+1" "A=M-1" "M=D" "@1" "D=A" "@LCL" "A=D+M" "D=M" "@SP"
            "M=M+1" "A=M-1" "M=D"]))))

(deftest return
  (testing "write code for return"
    (is (= (parse ["return"])
           ["@LCL" "D=M" "@R13" "M=D"
            "@5" "A=D-A" "D=M" "@R14" "M=D"
            "@SP" "AM=M-1" "D=M"
            "@ARG" "A=M" "M=D" "D=A" "@SP" "M=D+1" "@R13" "AM=M-1" "D=M"
            "@THAT" "M=D" "@R13" "AM=M-1" "D=M"
            "@THIS" "M=D" "@R13" "AM=M-1" "D=M"
            "@ARG" "M=D" "@R13" "AM=M-1" "D=M"
            "@LCL" "M=D"
            "@R14" "A=M" "0;JMP"]))))

(deftest call
  (testing "write code for call"
    (let [call-code (parse ["call main.test 2"])
          filtered-code (filter #(not (re-find #"RET_" %)) call-code)]
      (is (= filtered-code
             ["@SP" "D=M-1" "D=D-1" "D=D-1" "@R13" "M=D" "D=A" "@SP" "M=M+1" "A=M-1"
              "M=D"
              "@LCL" "D=M" "@SP" "M=M+1" "A=M-1" "M=D"
              "@ARG" "D=M" "@SP" "M=M+1" "A=M-1" "M=D"
              "@THIS" "D=M" "@SP" "M=M+1" "A=M-1" "M=D"
              "@THAT" "D=M" "@SP" "M=M+1" "A=M-1" "M=D"
              "@R13" "D=M+1" "@ARG" "M=D" "@SP" "D=M" "@LCL" "M=D"
              "@main.test" "0;JMP"]))
      )))

;;;;;;;;;;;;;;;;;;;;;;;; boot strap ;;;;;;;;;;;;;;;;;;;;;
(deftest bootstrap
  (testing "write code for boot strap"
    (is (= (write-bootstrap)
           ["@256" "D=A" "@SP" "M=D"
            "@bootstrap_return" "D=A" "@SP" "M=M+1" "A=M-1" "M=D"
            "@LCL" "D=M" "@SP" "M=M+1" "A=M-1" "M=D"
            "@ARG" "D=M" "@SP" "M=M+1" "A=M-1" "M=D"
            "@THIS" "D=M" "@SP" "M=M+1" "A=M-1" "M=D"
            "@THAT" "D=M" "@SP" "M=M+1" "A=M-1" "M=D"
            "@Sys.init" "0;JMP" "(bootstrap_return)"]))))

;;;;;;;;;;;;;;;;;;;;;;;; Example programs ;;;;;;;;;;;;;;;;;;;;;

(deftest simple-add
  (testing "write code add two numbers"
    (is (= (parse ["push constant 7" "push constant 8" "add"])
           ["@7" "D=A" "@SP" "M=M+1" "A=M-1" "M=D"
            "@8" "D=A" "@SP" "M=M+1" "A=M-1" "M=D"
            "@SP" "M=M-1" "A=M" "D=M" "A=A-1" "M=D+M"]))))

