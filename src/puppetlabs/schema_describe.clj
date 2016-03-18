(ns puppetlabs.schema-describe
  (:require [schema.core :as sc]
            [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.walk :as walk]))

(defn refine-instance
  [c]
  (let [name (.getCanonicalName ^java.lang.Class c)]
    (-> name
      (str/replace "java.lang." "")
      (str/replace "java.util." ""))))

(defn expected
  [[comparison condition]]
  {:pre [(= 'not comparison)
         (>= (count condition) 2)]}
  (case (first condition)
    instance? [:instance (second condition)]
    map? [:map]
    sequential? [:sequential]
    set? [:set]
    integer? [:integer]
    keyword? [:keyword]
    symbol? [:symbol]
    [:other (butlast condition)]))

(defn java-class->json-type
  [c]
  (let [ancs (ancestors c)]
    (cond
      (nil? c)
      "Null"

      (contains? ancs java.lang.Number)
      "Number"

      (contains? ancs java.util.Map)
      "Object"

      (contains? ancs java.util.Collection)
      "Array"

      (= java.lang.String c)
      "String"

      (contains? ancs clojure.lang.Named)
      "String"

      :else
      (format "String (%)" (refine-instance c)))))

(defn expected->json-type
  [[expected-kw arg]]
  (case expected-kw
    :instance (java-class->json-type arg)
    :map "Object"
    (:sequential :set) "Array"
    :integer "Integer"
    (:keyword :symbol) "String"
    :other (str arg)))

(defn actual
  [[comparison condition]]
  {:pre [(= 'not comparison)
         (>= (count condition) 2)]}
  (last condition))

(defn type-mismatch-msg
  [expected-type value]
  (format "Expected %s, got %s: %s"
          expected-type
          (-> value class java-class->json-type)
          (json/encode value)))

(defn describe-as-json
  [explanation]
  (cond
    (= 'disallowed-key explanation)
    "This key is not allowed."

    (= 'missing-required-key explanation)
    "This key is required."

    (= 'invalid-key explanation)
    "This key is the wrong type."

    (and (sequential? explanation)
         (= 'not (first explanation)))
    (let [expected (expected->json-type (expected explanation))
          actual (actual explanation)]
      (type-mismatch-msg expected actual))))

(comment
  (def explain-schema-errors #'puppetlabs.schema-tools/explain-schema-errors)

  (defn demo-explanation
    [schema value]
    (let [explanation (->> value
                        (sc/check schema)
                        explain-schema-errors)]
      (walk/prewalk (fn [x]
                      (or (describe-as-json x)
                          x))
                    explanation)))

  (demo-explanation
   {:specific sc/Str}
   {:specific 12})

  (demo-explanation
   [sc/Str]
   "admin")

  (demo-explanation
   {:top-level {:nested-str sc/Str
                :nested-int sc/Int}
    :other-key sc/Str}

   {:top-level {:nested-str 12
                :nested-int 400}
    :other-key "hello"})

  (demo-explanation
   [sc/Str]
   ["fine" 666])

  (demo-explanation
   [(sc/eq "good")]
   ["good" "good" "bad" "good"])

  (demo-explanation
   (sc/pred even?)
   3)

  )
