(ns starburst.core
  (:require
    [starburst.convert :refer :all]
    [starburst.header :refer [create-namespace, add-orion-dependency]]
    [starburst.spec :refer [add-spec-definition, declaration->spec]]
    [starburst.wrapper :refer [add-function-with-spec]]
    [clojure.java.io :as io]
    [clojure.pprint :refer [pprint]]
    [cheshire.core :as puss])
  (:gen-class))

(defn write-line
  [writer]
  (fn [sym]
    (pprint sym writer)
    (.newLine writer)))

(defn print-file-struct
  [file-struct writer]
  (let [writeln (write-line writer)]
    (writeln (:warning file-struct))
    (writeln `(ns ~(-> file-struct :name symbol) (:require ~@(seq (:dependencies file-struct)))))
    (->> (:definitions file-struct) (map writeln) dorun)
    (when (:function-name file-struct)
      (writeln `(defn ~(:function-name file-struct)
                  ~(:function-signature file-struct)
                  ~(:function-body file-struct)))
      (writeln (:function-spec file-struct))
      (writeln (:function-instrument file-struct)))))

(defn form
  [vl-schema-path orion-path]
  (with-open [primitives-writer (io/writer (io/file orion-path "primitives.clj"))]
    (-> (create-namespace "primitives")
        (add-spec-definition "boolean" '(partial instance? Boolean))
        (add-spec-definition "integer" 'integer?)
        (add-spec-definition "number" 'number?)
        (add-spec-definition "null" 'nil?)
        (add-spec-definition "string" 'string?)
        (print-file-struct primitives-writer)))

  (let [vl-schema (-> vl-schema-path slurp puss/parse-string)]
    (doseq [definition (vl-schema "definitions")]
      (with-open [definition-writer
                  (io/writer (io/file orion-path (str (-> definition key (reformat-name :sep \_)) ".clj")))]
        (let [def-name (-> definition key reformat-name)
              def-map (val definition)
              file-struct (create-namespace def-name)
              [fs spec] (declaration->spec (add-orion-dependency file-struct "primitives")
                                           def-map)]
          (->
            (add-spec-definition fs def-name spec)
            (add-function-with-spec def-name
                                    (def-map "properties")
                                    (def-map "required"))
            (print-file-struct definition-writer)))))))

(defn -main
  [& args]
  (form (nth args 0) (nth args 1)))
