(ns us.bpsm.sft
  (:refer-clojure :exclude [slurp])
  (:require [clojure.string :as string]
            [clojure.java.io :as jio])
  (:import [java.net URL]))

(defn parse-template
  "Parse the template txt, producing a sequence of strings and keywords.

The keywords are named by regions of txt delimited by « and »."
  [txt]
  (->> (clojure.string/split txt #"[«»]")
       (partition 2 2 nil)
       (map (fn [[s k]] (if-not k [s] [s (keyword k)])))
       (flatten)
       (remove #(or (nil? %) (= "" %)))))

(defn ex-missing-param
  "Construct an ex-info to report a missing template parameter, i.e.
When we try to apply a template requiring some paramter :A to a
param-fn which return nil for :A. "
  [param-name param-fn]
  (ex-info "Required template parameter is missing."
           {::param-name param-name
            ::param-fn param-fn}))

(defn apply-template [parsed-template param-fn]
  "Apply the parsed-template by calling f to provide the missing values.

parsed-template is a sequence of strings and keywords.
param-fn is a function that maps from keywords to strings. The result of
    apply-template replaces each keyword in template with the string
    returned by param-fn for that keyword. param-fn returning nil to
    indicate that it knows no value for a given keyword will cause
    apply-template to throw an exception.

The result of apply-template is a string."
  (->> (for [piece parsed-template]
           (if-not (keyword? piece)
             piece
             (let [param-value (param-fn piece)]
               (if (nil? param-value)
                 (throw (ex-missing-param piece param-fn))
                 param-value))))
       (apply str))) 

(defn template-fn*
  "Given a template as a string, return a function which accepts a param-fn
and returns a string with all params in template replaced by values provided
by param-fn."
  [template]
  (partial apply-template (parse-template template)))

(defmulti slurp
  (fn [source-type source] source-type))

(defmethod slurp :from-url
  [_ source]
  (clojure.core/slurp (URL. source)))

(defmethod slurp :from-resource 
  [_ source]
  (clojure.core/slurp (jio/resource source)))

(defmethod slurp :from-file
  [_ source]
  (clojure.core/slurp (jio/file source)))

(defmethod slurp :from-string
  [_ source]
  source)

(defmacro template-fn
  "Create a function of one argument which implements the tempalte identified by
source-type and source.

If source-type and source are both literals, then the template will be slurped
and parsed at compile time in order to emit a more efficient implementation of
the resulting template-fn.

Otherwise, this macro emits code which calls slurp and template-fn* at runtime."
  [source-type source]
  (if-not (and (keyword? source-type) (string? source))
    `(template-fn* (slurp ~source-type ~source))
    (let [parsed-template (parse-template (slurp source-type source))
          params (set (filter keyword? parsed-template))
          param-fn (gensym)]
      `(fn [~param-fn]
         (let [~@(interleave 
                  (for [p params]  (symbol (name p)))
                  (for [p params] `(~param-fn ~p)))]
           ~@(when clojure.core/*assert*
               (for [p params]
                 `(when (nil? ~(symbol (name p)))
                    (throw (ex-missing-param ~p ~param-fn)))))
           (str ~@(for [p parsed-template]
                    (if (keyword? p)
                      (symbol (name p))
                      p))))))))
