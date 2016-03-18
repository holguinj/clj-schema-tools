(ns puppetlabs.schema-describe-test
  (:require [puppetlabs.schema-describe :refer :all]
            [puppetlabs.schema-tools :as st]
            [clojure.test :refer :all]
            [schema.core :as sc]))

(deftest type-name-test
  (are [c expected] (= expected (type-name c))
    "hello" "String"
    123 "Long"
    1.23 "Double"
    true "Boolean"
    [] "clojure.lang.PersistentVector"))

(deftest type-mismatch?-test
  (let [check (comp #'st/->client-explanation sc/check)]
    (is (true? (type-mismatch? (check sc/Str 1))))
    (is (false? (type-mismatch? (check {:a sc/Str} {}))))
    (is (true? (type-mismatch? (check sc/Int nil))))

    ))
