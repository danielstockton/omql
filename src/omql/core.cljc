(ns omql.core
  (:refer-clojure :exclude [read])
  (:require #?(:clj [clojure.spec.alpha :as s]
               :cljs [cljs.spec.alpha :as s])
            [clojure.string :as str]))

(s/def ::ident (s/and vector? (s/cat :ident keyword? :value #(not (coll? %)))))
(s/def ::join-key (s/or :prop keyword? :ident ::ident))
(s/def ::join (s/and (s/map-of ::join-key ::query) #(= (count %) 1)))
(s/def ::union (s/and (s/map-of keyword? ::query) #(> (count %) 1)))

(s/def ::param-expr
  (s/cat :query-expr ::query-expr
         :params map?))

(s/def ::mutation-expr
  (s/or :no-params (s/cat :mutate-key symbol?)
        :with-params (s/cat :mutate-key symbol?
                            :params map?)))

(s/def ::query-expr
  (s/or :prop keyword?
        :ident ::ident
        :mutation-expr ::mutation-expr
        :union ::union
        :join  ::join
        :param-expr ::param-expr))

(s/def ::query
  (s/or :recursion (s/or :depth number?
                         :unbounded #(= % '...))
        :query     (s/and vector?
                          (s/+ ::query-expr))))

(defn tokenize [i]
  (cond
    (keyword? i)
      (name i)
    (symbol? i)
      (name i)
    :else
      i))

(defn- args [params]
  (let [params (map #(str (tokenize (first %)) ": " (second %)) params)]
    (str "(" (str/join ", " params) ")")))

(defn- read [prop params]
  (let [prop (tokenize prop)]
    (if (empty? params)
      prop
      (str prop (args params)))))

(declare graphql*)

(defn- join [join params]
  (let [[k q] (first (vec join))
        k     (tokenize k)]
    (if (empty? params)
      (str k (graphql* q))
      (let [{:keys [first after]} params
            node                  (graphql* q)]
        (if first
          (str k (args params)
               "{totalCount, pageInfo{hasNextPage, endCursor}, edges{cursor, node"
               node
               "}}")
          (str k (args params) node))))))

(defn- mutation [[type {:keys [mutate-key params]}]]
  (str (tokenize mutate-key) (args params)))

(defn fragment
  ([[type val]]
   (fragment [type val] nil))
  ([[type val] params]
   (let [alias  (:alias params)
         params (cond-> params
                  alias (dissoc params :alias))
         result (condp = type
                  :join
                    (join val params)
                  :prop
                    (read val params)
                  :param-expr
                    (let [{:keys [query-expr params]} val]
                      (fragment query-expr params))
                  :mutation-expr
                    (mutation val))]
     (if alias
       (str (tokenize alias) ": " result)
       result))))

(defn graphql*
  "Returns the GraphQL corresponding to a conformed query."
  [q]
  (str "{" (str/join ", " (map fragment (second q))) "}"))

(defn gather-parts [q]
  (reduce (fn [acc frag]
            (if (= (first frag) :mutation-expr)
              (update acc :mutations #(conj % frag))
              (update acc :reads #(conj % frag))))
          {:mutations []
           :reads     []}
          (second q)))

(defn graphql
  "Returns a GraphQL query for the given Om query."
  [q]
  (let [q* (s/conform ::query q)]
    (if (= q* ::s/invalid)
      (let [msg "Invalid query!"]
        #?(:clj (throw (Exception. msg))
           :cljs (throw (js/Error. msg))))
      (let [{:keys [reads mutations]} (gather-parts q*)]
        (str
         (if-not (empty? reads)
           (str "query " (graphql* [:query reads])))
         (if-not (empty? mutations)
           (str "mutation " (graphql* [:query mutations]))))))))

(comment
  (def q `[:read
           (:pread {:p1 1})
           {:join [:p1 :p2]}
           ({:pjoin [:r1 (:pread {:p1 1})]} {:first 1})
           ({:pjoin [:r1 :r2 :r3]} {:alias :rename})
           (mutate {:p1 1})])

  (graphql q)

  )
