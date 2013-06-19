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

(defmulti slurp
  (fn [source-type source] source-type))

(defmethod slurp :url
  [_ source]
  (clojure.core/slurp (URL. source)))

(defmethod slurp :rsrc 
  [_ source]
  (clojure.core/slurp (jio/resource source)))

(defmethod slurp :file
  [_ source]
  (clojure.core/slurp (jio/file source)))

(defmethod slurp :string
  [_ source]
  source)

(defn template-fn*
  "Given a template as a string, return a function which accepts a param-fn
and returns a string with all params in template replaced by values provided
by param-fn.

source-type is a keyword for which an implementation of us.bpsm.sft/slurp
is defined."
  ([template]
     (partial apply-template (parse-template template)))
  ([source-type source]
     (template-fn* (slurp source-type source))))

(defn- literals-length 
  "The sum of the lengths of all strings in the parsed template. (This
  is the length of the template's expansion provided all parameters
  evaluate to the emtpy string.)" 
  [parsed-template]
  (->> parsed-template
       (filter string?)
       (map count)
       (reduce + 0)))

(defn- keyword->symbol 
  [k]
  (symbol (name k)))

(defn- params
  "Return a map of param names (keywords) to symbols with the same name."
  [parsed-template]
  (->> parsed-template
       (filter keyword?)
       (map #(vector % (keyword->symbol %)))
       (into {})))

(defn- param-counts
  "Return a map of param names (keywords) to the number of times they
appear in parsed-template."
  [parsed-template]
  (->> parsed-template
       (filter keyword?)
       frequencies))

(defn- expected-length-expression
  "Return an arithmetic expression which will compute the exact length
of the expanded template, under the assumption that each of its parameters
will be bound to a symbol of the same name in the scope in which this
expression will be evaluated."
  [parsed-template]
  `(+ ~(literals-length parsed-template)
      ~@(for [[p n] (param-counts parsed-template)]
          (if  (= 1 n)
            `(count ~(keyword->symbol p))
            `(* ~n (count ~(keyword->symbol p)))))))

(defmacro template-fn
  "Create a function of one argument which implements the tempalte
identified by source-type and source.

If called with two arguments: source-type and source that are both
literals, the template will be slurped and parsed at compile time in
order to emit a more efficient implementation of the resulting
template-fn.

If called with a single argument: template that is a string literal,
it will be parsed at compile time in order to emit a more efficient
implementation of the resulting template-fn.

Otherwise, this macro emits code which calls slurp and template-fn* at
runtime.

source-type is a keyword for which an implementation of us.bpsm.sft/slurp
is defined."
  ([template]
     (if-not (string? template)
       `(template-fn* ~template)
       (let [parsed-template (parse-template template)
             params (params parsed-template)
             param-fn (gensym)]
         `(fn [~param-fn]
            (let [~@(interleave
                     (for [[k s] params]  s)
                     (for [[k s] params] `(~param-fn ~k)))]
              ~@(when clojure.core/*assert*
                  (for [[k s] params]
                    `(when (nil? ~s)
                       (throw (ex-missing-param ~k ~param-fn)))))
              (str
               (doto (StringBuilder. ~(expected-length-expression parsed-template))
                 ~@(for [p parsed-template]
                     `(.append ~(if (keyword? p)
                                  (params p)
                                  p))))))))))
  ([source-type source]
     (if-not (and (keyword? source-type) (string? source))
       `(template-fn* ~source-type ~source)
       `(template-fn ~(slurp source-type source)))))
