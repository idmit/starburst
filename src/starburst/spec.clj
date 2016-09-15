(ns starburst.spec
  (:require [starburst.convert :refer :all]
            [starburst.header :refer [add-orion-dependency]]
            [clojure.string :as str])
  )

(defn add-spec-definition
  [file-struct name spec-form]
  (update-in file-struct [:definitions] conj `(clojure.spec/def ~(str->short-ns-kword name) ~spec-form)))

(declare declaration->spec)

(defn resolve-enum-spec
  [file-struct m]
  [file-struct (into #{} (m "enum"))])

(defn resolve-ref-spec
  [file-struct m]
  (let [ref-spec-key (-> (m "$ref")
                         (str/split #"/")
                         last)
        ref-spec-name (reformat-name ref-spec-key)
        module-name (reformat-name ref-spec-key :sep \_)]
    [(add-orion-dependency file-struct ref-spec-name) (str->ns-kword ref-spec-name :nms module-name)]))

(defn resolve-number-spec
  [file-struct m]
  (let [min (m "minimum")
        max (m "maximum")
        type-spec (str->ns-kword (m "type"))
        anon-lt #(str "#(<= " %1 " " %2 ")")
        suffix (cond-> '()
                       max (conj (symbol (anon-lt "%" max)))
                       min (conj (symbol (anon-lt min "%"))))]
    [file-struct (if (seq suffix)
                   (concat `(clojure.spec/and) `(~type-spec) suffix)
                   type-spec)]))

(defn resolve-one-of-spec
  [file-struct m]
  (let [nats (->> (iterate inc 0) (map (comp keyword str)))
        [fs specs] (reduce (fn [[in-fs spcs] in-m]
                             (let [[fs-updated sym] (declaration->spec in-fs in-m)]
                               [fs-updated (conj spcs sym)]))
                           [file-struct []]
                           (m "oneOf"))
        pairs (map list nats specs)]
    [fs (reduce concat `(clojure.spec/alt) pairs)]))

(defn resolve-array-spec
  [file-struct m]
  (let [[fs sym] (declaration->spec file-struct (m "items"))
        min-count (when-let [min-items (m "minItems")]
                    `(:min-count ~min-items))
        max-count (when-let [max-items (m "maxItems")]
                    `(:max-count ~max-items))]
    [fs `(clojure.spec/coll-of ~sym :kind vector? ~@min-count ~@max-count)]))

(defn resolve-property-spec
  [file-struct [k v]]
  (let [[fs sym] (declaration->spec file-struct v)]
    (add-spec-definition fs (reformat-name k) sym)))

(defn resolve-object-spec
  [file-struct m]
  (let [complete-fs (reduce resolve-property-spec
                            file-struct
                            (m "properties"))]
    [complete-fs `(clojure.spec/keys :req [~@(map (comp str->short-ns-kword reformat-name) (m "required"))])]))

(defn resolve-spec-by-type
  [file-struct m]
  (case (m "type")
    "object" (resolve-object-spec file-struct m)
    "array" (resolve-array-spec file-struct m)
    "number" (resolve-number-spec file-struct m)
    [file-struct (str->ns-kword (m "type"))]))

(defn declaration->spec
  [file-struct m]
  (cond
    (m "enum") (resolve-enum-spec file-struct m)
    (m "oneOf") (resolve-one-of-spec file-struct m)
    (m "$ref") (resolve-ref-spec file-struct m)
    (m "type") (resolve-spec-by-type file-struct m)
    :else [file-struct `(constantly true)]))
