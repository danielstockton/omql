(ns omql.core-test
  (:require [clojure.test :refer :all]
            [omql.core :refer [graphql]]))

(deftest graphql-test
  (testing "Reads"
    (is (= (graphql '[:name]) "query {name}"))
    (is (= (graphql '[(:name {:p1 1})]) "query {name(p1: 1)}")))
  (testing "Joins"
    (is (= (graphql '[{:j1 [:p1 :p2]}]) "query {j1{p1, p2}}"))))
