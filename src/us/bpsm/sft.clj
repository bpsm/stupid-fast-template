(ns us.bpsm.sft
  (:require [clojure.string :as string]))

;;; What a template is:
;;;
;;; A template is a function which takes a function mapping keywords
;;; to strings and returns a string.
;;;
;;; We write a template as a string thus:
;;;
;;;    Hello World!
;;;
;;; This template is equivalent to this function:
;;;
;;;    (fn [_] "Hello World!")
;;;
;;; Templates can use named parameters, as in this example:
;;;
;;;    Hello «Name»!
;;;
;;; This template is equivalent to this function:
;;;
;;;    (fn [f] (str "Hello " (f :Name) "!"))
;;;
;;; Templates, since they are just text, can be specified in code as
;;; string literals. They can also be loaded from files.

(defn parse-template
  "Parse the template txt, producing a sequence of strings and keywords."
  [txt]
  (->> (clojure.string/split txt #"[«»]")
       (partition 2 2 nil)
       (map (fn [[s k]] (if-not k [s] [s (keyword k)])))
       (flatten)
       (remove #(or (nil? %) (= "" %)))))

(defn apply-template [parsed-template param-fn]
  "Apply the parsed-template by calling f to provide the missing values.

parsed-template is a sequence of strings and keywords.
param-fn is a function that maps from keywords to strings. The result of
    apply-template replaces each keyword in template with the string
    returned by param-fn for that keyword. param-fn returning nil to
    indicate that it knows no value for a given keyword will cause
    apply-template to throw an exception.

The result of apply-template is a string."
  (apply str 
         (for [piece parsed-template]
           (if-not (keyword? piece)
             piece
             (if-let [param-value (param-fn piece)]
               param-value
               (throw (ex-info "Unsupported template parameter" 
                               {::param-name piece
                                ::param-fn param-fn})))))))
