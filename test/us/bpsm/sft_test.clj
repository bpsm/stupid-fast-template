(ns us.bpsm.sft-test
  (:refer-clojure :exclude [slurp])
  (:use clojure.test
        us.bpsm.sft))

(deftest test-parse-template
  (are [x y] (= x (parse-template y))
       [] ""
       ["Hello World!"] "Hello World!"
       ["Hello " :Name "!"] "Hello «Name»!"
       [:Greeting " " :Name "!"] "«Greeting» «Name»!"
       [:Greeting :Name] "«Greeting»«Name»"))

(deftest test-apply-template
  (are [x y z] (= x (apply-template (parse-template y) z))
       "" "" {}
       "Hello World!" "Hello World!" {}
       "Hello Ben!" "Hello «Name»!" {:Name "Ben"}
       "Hello Ben!" "«Greeting» «Name»!" {:Greeting "Hello" :Name "Ben"}
       "bigLITTLE" "«BIG»«little»" {:BIG "big" :little "LITTLE"}))

(deftest test-template-fn*
  (are [result template param-fn] (= result ((template-fn* template) param-fn))
       "" "" {}
       "Hello World!" "Hello World!" {}
       "Hello Ben!" "Hello «Name»!" {:Name "Ben"}
       "Hello Ben!" "«Greeting» «Name»!" {:Greeting "Hello" :Name "Ben"}
       "bigLITTLE" "«BIG»«little»" {:BIG "big" :little "LITTLE"}))

(deftest test-template-fn
  (are [result template param-fn] 
    (= result ((template-fn :string template) param-fn))
    "" "" {}
    "Hello World!" "Hello World!" {}
    "Hello Ben!" "Hello «Name»!" {:Name "Ben"}
    "Hello Ben!" "«Greeting» «Name»!" {:Greeting "Hello" :Name "Ben"}
    "bigLITTLE" "«BIG»«little»" {:BIG "big" :little "LITTLE"}))
